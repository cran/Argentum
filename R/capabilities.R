# Capabilities -----------------------------------------------------------

WFS_VERSIONS <- c("2.0.0", "1.1.0", "1.0.0")
WMS_VERSIONS <- c("1.3.0", "1.1.1")

#' Retrieve and parse a GetCapabilities document
#'
#' Negotiates the protocol version by trying the newest first and falling back,
#' which is what makes the package work against both GeoServer and ArcGIS
#' Server deployments. The parsed document is cached; see
#' [argentum_cache_path()].
#'
#' @param x An organization name from [argentum_organizations()], or a service
#'   URL. Any query string on the URL is discarded and rebuilt.
#' @param service `"wfs"` or `"wms"`.
#' @param version Force a protocol version, e.g. `"1.1.0"`. If `NULL`
#'   (default) versions are tried newest-first.
#' @param refresh Ignore the cache and re-request the document.
#' @return An object of class `argentum_capabilities`: an `xml_document` with
#'   `service`, `version` and `url` attributes.
#' @export
#' @examples
#' \dontrun{
#' cap <- argentum_capabilities("https://wms.ign.gob.ar/geoserver/ows", "wfs")
#' cap
#' }
argentum_capabilities <- function(x,
                                  service = c("wfs", "wms"),
                                  version = NULL,
                                  refresh = FALSE) {
  service <- match.arg(service)
  url <- arg_resolve_endpoint(x, service)
  base <- arg_strip_ogc_params(arg_ensure_scheme(arg_clean_url(url)))

  versions <- version %||% switch(service, wfs = WFS_VERSIONS, wms = WMS_VERSIONS)

  key <- arg_cache_key("cap", base, service, versions)
  compute <- function() arg_fetch_capabilities(base, service, versions)

  # The cache stores the document's text, not the parsed document: an
  # `xml_document` is an external pointer, `saveRDS()` writes it as NULL, and
  # a later session reading that file back gets an object whose every method
  # fails with "external pointer is not valid".
  record <- arg_cached(key = key, refresh = refresh, compute = compute)

  # A cache written by an earlier build held the parsed document; treat those
  # entries as stale and recompute rather than handing back a corpse.
  if (!is.list(record) || !rlang::is_string(record$text %||% NULL)) {
    record <- arg_cached(key = key, refresh = TRUE, compute = compute)
  }

  arg_build_capabilities(record)
}

#' Rebuild the capabilities object from its cached record
#' @noRd
arg_build_capabilities <- function(record) {
  xml <- arg_read_xml(record$text)
  xml2::xml_ns_strip(xml)
  structure(
    xml,
    class = c("argentum_capabilities", class(xml)),
    service = record$service,
    version = record$version,
    url = record$url
  )
}

#' @noRd
arg_fetch_capabilities <- function(base, service, versions) {
  errors <- character()

  for (v in versions) {
    arg_inform("Requesting {toupper(service)} {v} capabilities from {.url {base}}")
    req <- arg_request(base, list(
      service = toupper(service),
      version = v,
      request = "GetCapabilities"
    ))
    out <- rlang::try_fetch({
      txt <- httr2::resp_body_string(arg_perform(req, "capabilities request"))
      xml <- arg_read_xml(txt)
      xml2::xml_ns_strip(xml)
      arg_check_exception(xml)
      list(text = txt, xml = xml)
    }, error = function(cnd) {
      errors[[v]] <<- conditionMessage(cnd)
      NULL
    })

    if (!is.null(out)) {
      # The server may downgrade us; record what it actually answered.
      served <- xml2::xml_attr(xml2::xml_root(out$xml), "version")
      return(list(
        text = out$text,
        service = toupper(service),
        version = served %|NA|% v,
        url = base
      ))
    }
  }

  cli::cli_abort(c(
    "Could not obtain {toupper(service)} capabilities from {.url {base}}.",
    "x" = "Tried version{?s} {.val {versions}}.",
    "i" = utils::tail(errors, 1)
  ), class = "argentum_error_capabilities")
}

#' @export
print.argentum_capabilities <- function(x, ...) {
  title <- xml2::xml_text(xml2::xml_find_first(x, paste0(
    arg_xpath(c("ServiceIdentification", "Service")), arg_xpath("Title", "/")
  )))
  cli::cli_text("{.strong <argentum_capabilities>}")
  cli::cli_bullets(c(
    "*" = "Service: {attr(x, 'service')} {attr(x, 'version')}",
    "*" = "Endpoint: {.url {attr(x, 'url')}}",
    "*" = "Title: {if (is.na(title)) '<none>' else title}"
  ))
  invisible(x)
}

# --- namespace-agnostic XPath --------------------------------------------
# xml2::xml_ns_strip() removes only *default* namespaces. Every GeoServer
# answers a WFS 2.0 GetCapabilities as <wfs:WFS_Capabilities xmlns:wfs=...>,
# so a plain "//FeatureType" matches nothing there and the service looks as if
# it published no layers. Matching on local names is what the QGIS plugin does,
# and it is the only thing that works across WFS 1.0.0/1.1.0/2.0.0 and WMS
# 1.1.1/1.3.0, which move both the namespace and the element names around.

#' XPath matching any of `names`, whatever namespace they carry
#'
#' @param names Element local names.
#' @param axis Prefix such as `"//"`, `"./"` or `".//"`.
#' @noRd
arg_xpath <- function(names, axis = "//") {
  paste0(axis, "*[", paste(sprintf("local-name()='%s'", names), collapse = " or "), "]")
}

#' XPath for an element that has a child `Name` with the given value
#' @noRd
arg_xpath_named <- function(element, value, axis = "//") {
  sprintf(
    "%s[*[local-name()='Name']=%s]",
    arg_xpath(element, axis), arg_xpath_literal(value)
  )
}

#' Quote a value for use inside an XPath expression
#' @noRd
arg_xpath_literal <- function(x) {
  if (!grepl("'", x, fixed = TRUE)) return(paste0("'", x, "'"))
  paste0("concat('", gsub("'", "', \"'\", '", x, fixed = TRUE), "')")
}

# --- coordinate reference systems ----------------------------------------

# Tried in this order when the caller does not name one, matching the QGIS
# plugin. EPSG:22185 is Gauss-Kruger zone 5, the most common projected CRS in
# Argentine provincial services; EPSG:900913 is the pre-standard spelling of
# web mercator that older MapServer installs still publish.
#' @noRd
ARG_PREFERRED_CRS <- c("EPSG:4326", "EPSG:3857", "EPSG:900913", "EPSG:22185")

#' Accept `EPSG:4326`, URNs and OGC http identifiers alike
#' @noRd
arg_normalize_crs <- function(value) {
  text <- trimws(as.character(value))
  text[is.na(text)] <- ""

  vapply(text, function(one) {
    if (!nzchar(one)) return("")
    if (grepl("^EPSG:", one, ignore.case = TRUE)) {
      return(paste0("EPSG:", sub("^.*:", "", one)))
    }
    if (grepl("epsg", one, ignore.case = TRUE)) {
      code <- sub("^.*:", "", sub(":+$", "", one))
      if (grepl("^[0-9]+$", code)) return(paste0("EPSG:", code))
    }
    if (grepl("^http", one) && grepl("/def/crs/EPSG/", one, fixed = TRUE)) {
      code <- sub("^.*/", "", sub("/+$", "", one))
      if (grepl("^[0-9]+$", code)) return(paste0("EPSG:", code))
    }
    one
  }, character(1), USE.NAMES = FALSE)
}

#' Every CRS a layer declares, in declaration order
#'
#' WMS inherits CRS down the layer tree, so the ancestors count too. WFS moves
#' the element name around between versions (`SRS`, `DefaultSRS`, `DefaultCRS`,
#' `OtherCRS`), which is why these are matched by name rather than by path.
#' @noRd
arg_layer_crs_list <- function(cap, layer, service = c("wfs", "wms")) {
  service <- match.arg(service)

  xpath <- if (service == "wfs") {
    paste0(
      arg_xpath_named("FeatureType", layer),
      arg_xpath(c("DefaultCRS", "DefaultSRS", "OtherCRS", "OtherSRS", "SRS", "CRS"), "/")
    )
  } else {
    paste0(
      arg_xpath_named("Layer", layer),
      "/ancestor-or-self::*[local-name()='Layer']",
      arg_xpath(c("CRS", "SRS"), "/")
    )
  }

  codes <- unlist(strsplit(xml2::xml_text(xml2::xml_find_all(cap, xpath)), "[[:space:]]+"))
  codes <- arg_normalize_crs(codes[nzchar(codes)])
  unique(codes[nzchar(codes)])
}

#' Pick a CRS the layer actually publishes
#'
#' @param available Codes taken from the capabilities document.
#' @param preferred Tried before the package's own preference list.
#' @return A CRS string, or `NULL` when the layer declares none, in which case
#'   the caller should let the server use its default rather than invent one.
#' @noRd
arg_choose_crs <- function(available, preferred = NULL) {
  available <- available[nzchar(available)]
  if (!length(available)) return(NULL)

  candidates <- c(preferred, ARG_PREFERRED_CRS)
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]

  hit <- match(toupper(candidates), toupper(available))
  hit <- hit[!is.na(hit)]
  if (length(hit)) available[hit[1]] else available[1]
}

# --- image formats --------------------------------------------------------

#' @noRd
ARG_PREFERRED_FORMATS <- c("image/png", "image/png8", "image/png24", "image/jpeg", "image/gif")

#' Formats the service advertises for GetMap
#' @noRd
arg_wms_formats <- function(cap) {
  nodes <- xml2::xml_find_all(cap, paste0(
    arg_xpath("Capability"), arg_xpath("Request", "/"),
    arg_xpath("GetMap", "/"), arg_xpath("Format", "/")
  ))
  formats <- trimws(xml2::xml_text(nodes))
  unique(formats[nzchar(formats)])
}

#' Pick an image format the service actually offers
#' @noRd
arg_choose_format <- function(available, preferred = NULL) {
  available <- available[nzchar(available)]
  if (!length(available)) return("image/png")

  candidates <- c(preferred, ARG_PREFERRED_FORMATS)
  hit <- match(tolower(candidates), tolower(available))
  hit <- hit[!is.na(hit)]
  if (length(hit)) return(available[hit[1]])

  images <- available[grepl("^image/", available, ignore.case = TRUE)]
  if (length(images)) images[1] else available[1]
}

#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' @noRd
`%|NA|%` <- function(x, y) if (length(x) != 1 || is.na(x) || !nzchar(x)) y else x
