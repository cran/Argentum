# WFS --------------------------------------------------------------------

#' List the layers published by a service
#'
#' @param x An organization name from [argentum_organizations()], or a service
#'   URL.
#' @param service `"wfs"` (default) or `"wms"`.
#' @param refresh Ignore the cached capabilities document.
#' @return A data frame with columns `name`, `title`, `abstract`, `crs` and
#'   `bbox` (a WGS84 `xmin,ymin,xmax,ymax` string). Zero rows if the service
#'   publishes none. `crs` is normalised to `EPSG:code` form even when the
#'   service declares it as a URN.
#' @export
#' @examples
#' \dontrun{
#' layers <- argentum_layers("https://wms.ign.gob.ar/geoserver/ows")
#' head(layers[, c("name", "title")])
#' }
argentum_layers <- function(x, service = c("wfs", "wms"), refresh = FALSE) {
  service <- match.arg(service)
  cap <- argentum_capabilities(x, service = service, refresh = refresh)
  if (service == "wfs") arg_parse_wfs_layers(cap) else arg_parse_wms_layers(cap)
}

#' @noRd
arg_parse_wfs_layers <- function(cap) {
  nodes <- xml2::xml_find_all(cap, arg_xpath("FeatureType"))
  if (!length(nodes)) return(arg_empty_layers())

  bbox <- vapply(nodes, function(n) {
    lower <- xml2::xml_text(xml2::xml_find_first(n, arg_xpath("LowerCorner", ".//")))
    upper <- xml2::xml_text(xml2::xml_find_first(n, arg_xpath("UpperCorner", ".//")))
    if (is.na(lower) || is.na(upper)) {
      # WFS 1.0.0 spells it differently
      lat <- xml2::xml_find_first(n, arg_xpath("LatLongBoundingBox", ".//"))
      if (is.na(lat)) return(NA_character_)
      return(paste(xml2::xml_attr(lat, c("minx", "miny", "maxx", "maxy")), collapse = ","))
    }
    paste(gsub(" ", ",", c(lower, upper)), collapse = ",")
  }, character(1))

  data.frame(
    name     = arg_node_text(nodes, arg_xpath("Name", "./")),
    title    = arg_node_text(nodes, arg_xpath("Title", "./")),
    abstract = arg_node_text(nodes, arg_xpath("Abstract", "./")),
    crs      = arg_normalize_crs(
      arg_node_text(nodes, arg_xpath(c("DefaultSRS", "DefaultCRS", "SRS"), "./"))
    ),
    bbox     = bbox,
    stringsAsFactors = FALSE
  ) |> arg_fill_titles()
}

#' @noRd
arg_node_text <- function(nodes, xpath) {
  vapply(nodes, function(n) xml2::xml_text(xml2::xml_find_first(n, xpath)), character(1))
}

#' @noRd
arg_fill_titles <- function(df) {
  df$title <- ifelse(is.na(df$title) | !nzchar(df$title), df$name, df$title)
  df <- df[!is.na(df$name) & nzchar(df$name), , drop = FALSE]
  rownames(df) <- NULL
  df
}

#' @noRd
arg_empty_layers <- function() {
  data.frame(
    name = character(), title = character(), abstract = character(),
    crs = character(), bbox = character(), stringsAsFactors = FALSE
  )
}

#' Read a WFS layer as an sf object
#'
#' Improves on the v1 importer in four ways: the protocol version is taken
#' from the negotiated capabilities rather than hard-coded, large layers are
#' fetched page by page instead of in one request that times out, a spatial
#' or attribute filter can be pushed down to the server, and if the endpoint
#' cannot produce GeoJSON the request falls back to GML.
#'
#' When a read fails outright the request is retried with paging disabled and
#' then against each older protocol version, exactly as the ArgentinaGeoServices
#' QGIS plugin does: several Argentine GeoServer installs advertise 2.0.0 and
#' then fail to serialise GML in that version while answering 1.0.0 perfectly.
#' If every attempt fails, the server is asked for a single feature so that its
#' own error message can be reported instead of a bare parse failure.
#'
#' @param x An organization name from [argentum_organizations()], or a WFS URL.
#' @param layer Layer (feature type) name, as reported by [argentum_layers()].
#' @param bbox Optional spatial filter. Either a numeric vector
#'   `c(xmin, ymin, xmax, ymax)`, or any object accepted by [sf::st_bbox()],
#'   such as an existing `sf` object.
#' @param crs Coordinate reference system to request, e.g. `"EPSG:4326"` or
#'   `4326`. `NULL` (default) negotiates one from the layer's declared list,
#'   preferring `EPSG:4326`; if the layer declares none, the server's own
#'   default is used.
#' @param max_features Stop after this many features. `NULL` (default) reads
#'   everything.
#' @param page_size Features per request. Defaults to
#'   `getOption("argentum.page_size")`. Set to `Inf` to disable paging for
#'   servers that mishandle `startIndex`.
#' @param filter A CQL filter pushed to the server, e.g.
#'   `"provincia = 'Mendoza'"`. Supported by GeoServer; ignored by some
#'   ArcGIS deployments.
#' @param quiet Suppress progress messages.
#' @return An `sf` data frame. Zero rows if the filter matches nothing.
#' @seealso [argentum_download()] to write layers straight to disk.
#' @export
#' @examples
#' \dontrun{
#' url <- "https://wms.ign.gob.ar/geoserver/ows"
#'
#' # Everything in a small layer
#' provinces <- argentum_read_wfs(url, "ign:provincia")
#'
#' # Only what falls inside a bounding box, reprojected
#' argentum_read_wfs(
#'   url, "ign:provincia",
#'   bbox = c(-59, -35, -57, -34),
#'   crs = 4326
#' )
#' }
argentum_read_wfs <- function(x,
                              layer,
                              bbox = NULL,
                              crs = NULL,
                              max_features = NULL,
                              page_size = arg_opt("argentum.page_size"),
                              filter = NULL,
                              quiet = arg_opt("argentum.quiet")) {
  if (!rlang::is_string(layer)) cli::cli_abort("{.arg layer} must be a single string.")

  cap <- argentum_capabilities(x, service = "wfs")
  base <- attr(cap, "url")
  version <- attr(cap, "version")

  # An explicit crs is honoured verbatim; otherwise negotiate from what the
  # layer declares, and if it declares nothing let the server decide.
  crs_str <- arg_crs_string(crs) %||% arg_choose_crs(arg_layer_crs_list(cap, layer, "wfs"))
  bbox_str <- arg_bbox_string(bbox, crs_str)

  plan <- arg_wfs_attempt_plan(version)
  problems <- character()

  for (attempt in plan) {
    out <- rlang::try_fetch(
      arg_wfs_collect(
        base = base, layer = layer, version = attempt$version,
        page_size = if (attempt$paging) page_size else Inf,
        crs_str = crs_str, bbox_str = bbox_str, filter = filter,
        max_features = max_features, quiet = quiet
      ),
      error = function(cnd) {
        problems[[length(problems) + 1L]] <<- conditionMessage(cnd)
        NULL
      }
    )

    if (!is.null(out)) {
      if (nzchar(attempt$label)) {
        arg_inform("Read {.field {layer}} with {attempt$label}.")
      }
      if (!quiet) cli::cli_alert_success("Read {nrow(out)} feature{?s} from {.field {layer}}.")
      return(out)
    }
  }

  says <- arg_wfs_explain(base, layer, version)
  cli::cli_abort(c(
    "Could not read layer {.field {layer}}.",
    "x" = if (nzchar(says)) "The server says: {says}",
    "i" = "Tried WFS {.val {unique(vapply(plan, function(a) a$version, character(1)))}}, with and without paging, as GeoJSON and as GML.",
    "i" = utils::tail(problems, 1)
  ), class = "argentum_error_read")
}

#' Attempts to make when a layer will not read, most capable first
#'
#' Dropping to an older version is not superstition: a service can advertise
#' 2.0.0, fail to encode GML in it, and answer 1.0.0 without complaint.
#'
#' The plugin has a fifth step, a hand-built `GetFeature` URL, as a way around
#' the QGIS provider's URI parsing. There is no equivalent here because every
#' request this package makes is already a plain URL.
#' @noRd
arg_wfs_attempt_plan <- function(version) {
  known <- match(version, WFS_VERSIONS, nomatch = 0L)
  older <- if (known > 0L) WFS_VERSIONS[-seq_len(known)] else WFS_VERSIONS

  plan <- list(
    list(version = version, paging = TRUE, label = ""),
    list(version = version, paging = FALSE, label = "paging disabled")
  )
  c(plan, lapply(older, function(v) {
    list(version = v, paging = FALSE, label = sprintf("WFS %s, paging disabled", v))
  }))
}

#' Page through a layer and bind the results
#' @noRd
arg_wfs_collect <- function(base, layer, version, page_size, crs_str, bbox_str,
                            filter, max_features, quiet) {
  count_param <- if (arg_version_gte(version, "2.0.0")) "count" else "maxFeatures"
  type_param <- if (arg_version_gte(version, "2.0.0")) "typeNames" else "typeName"

  if (!is.finite(page_size) || page_size <= 0) {
    page_size <- max_features %||% Inf
  }

  pieces <- list()
  read_so_far <- 0L
  page <- 0L

  repeat {
    this_page <- if (is.finite(page_size)) {
      min(page_size, (max_features %||% Inf) - read_so_far)
    } else {
      max_features
    }

    query <- list(
      service      = "WFS",
      version      = version,
      request      = "GetFeature",
      outputFormat = "application/json",
      srsName      = crs_str,
      bbox         = bbox_str,
      CQL_FILTER   = filter
    )
    query[[type_param]] <- layer
    if (is.finite(this_page %||% Inf)) query[[count_param]] <- format(this_page, scientific = FALSE)
    if (page > 0L) query[["startIndex"]] <- format(read_so_far, scientific = FALSE)

    if (!quiet) cli::cli_progress_step("Reading {.field {layer}} (page {page + 1L})")

    chunk <- arg_read_sf(base, query, layer)
    n <- nrow(chunk)
    if (n == 0L) break

    pieces[[length(pieces) + 1L]] <- chunk
    read_so_far <- read_so_far + n
    page <- page + 1L

    if (!is.finite(page_size)) break
    if (n < page_size) break                                   # server exhausted
    if (!is.null(max_features) && read_so_far >= max_features) break
  }

  if (!length(pieces)) {
    cli::cli_warn("{.field {layer}} returned no features for the given filters.")
    return(sf::st_sf(geometry = sf::st_sfc(), crs = crs_str))
  }

  if (length(pieces) == 1L) pieces[[1]] else do.call(rbind, pieces)
}

#' Build a request URL without duplicating anything already on the base
#' @noRd
arg_query_url <- function(base, query) {
  query <- Filter(Negate(is.null), query)
  base <- arg_drop_query_params(base, names(query))
  httr2::req_url_query(httr2::request(base), !!!query)$url
}

#' Read one page, falling back from GeoJSON to GML
#'
#' `sf::st_read()` rather than `sf::read_sf()` on purpose: `read_sf()` returns
#' a tibble and therefore needs the tibble package, which sf only *suggests*.
#' On a library without it, every attempt in the retry ladder died with
#' "package tibble not available" dressed up as a protocol failure.
#' @noRd
arg_read_sf <- function(base, query, layer) {
  out <- rlang::try_fetch(
    sf::st_read(arg_query_url(base, query), quiet = TRUE, stringsAsFactors = FALSE),
    error = function(cnd) NULL
  )
  if (!is.null(out)) return(out)

  # Some ArcGIS Server and older MapServer endpoints do not implement
  # outputFormat=application/json. GML is mandatory in the spec.
  query$outputFormat <- NULL
  arg_inform("GeoJSON unavailable for {.field {layer}}; retrying with GML.")

  rlang::try_fetch(
    sf::st_read(arg_query_url(base, query), quiet = TRUE, stringsAsFactors = FALSE),
    error = function(cnd) {
      cli::cli_abort(
        c("Could not read layer {.field {layer}}.",
          "i" = "Neither GeoJSON nor GML could be parsed from the endpoint."),
        parent = cnd, class = "argentum_error_read"
      )
    }
  )
}

#' Ask the server for one feature so it can say what is actually wrong
#'
#' When nothing reads, a parse failure is all the client can see. A one-feature
#' `GetFeature` usually comes back with the real reason - a layer that does not
#' exist, a CRS the service cannot reproject to, "Error encoding object to
#' xml-element" - and that is what the user needs in the message.
#' @noRd
arg_wfs_explain <- function(base, layer, version) {
  two_zero <- arg_version_gte(version, "2.0.0")

  query <- list(service = "WFS", request = "GetFeature", version = version)
  query[[if (two_zero) "typeNames" else "typeName"]] <- layer
  query[[if (two_zero) "count" else "maxFeatures"]] <- "1"

  resp <- try(
    httr2::req_perform(arg_request(base, query, max_tries = 1)),
    silent = TRUE
  )
  if (inherits(resp, "try-error")) return("")

  body <- try(httr2::resp_body_string(resp), silent = TRUE)
  if (inherits(body, "try-error") || !nzchar(body)) return("")
  arg_exception_text(body)
}

#' Text of an OGC exception report, or `""` for anything else
#' @noRd
arg_exception_text <- function(txt) {
  xml <- try(arg_read_xml(txt), silent = TRUE)
  if (inherits(xml, "try-error")) return("")
  xml2::xml_ns_strip(xml)

  root <- xml2::xml_name(xml2::xml_root(xml))
  if (!root %in% c("ExceptionReport", "ServiceExceptionReport")) return("")

  node <- xml2::xml_find_first(
    xml, arg_xpath(c("ExceptionText", "ServiceException", "Exception"))
  )
  if (is.na(node)) return("")
  substr(trimws(xml2::xml_text(node)), 1, 300)
}

#' @noRd
arg_crs_string <- function(crs) {
  if (is.null(crs)) return(NULL)
  if (is.numeric(crs)) return(paste0("EPSG:", as.integer(crs)))
  if (rlang::is_string(crs)) return(crs)
  cli::cli_abort("{.arg crs} must be an EPSG code or a CRS string.")
}

#' @noRd
arg_bbox_string <- function(bbox, crs = NULL) {
  if (is.null(bbox)) return(NULL)
  if (!is.numeric(bbox)) bbox <- as.numeric(sf::st_bbox(bbox))
  if (length(bbox) != 4L) cli::cli_abort("{.arg bbox} must have four values: xmin, ymin, xmax, ymax.")
  paste(c(format(bbox, scientific = FALSE), crs), collapse = ",")
}

#' @noRd
arg_version_gte <- function(a, b) {
  utils::compareVersion(a, b) >= 0
}
