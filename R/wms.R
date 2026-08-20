# WMS --------------------------------------------------------------------
# New in 2.0.0. v1 collected WMS_URL in the catalogue but never used it: the
# package advertised WMS support it did not have.

#' @noRd
arg_parse_wms_layers <- function(cap) {
  # Only leaf <Layer> nodes are requestable; parents are grouping containers.
  nodes <- xml2::xml_find_all(
    cap, paste0(arg_xpath("Layer"), "[*[local-name()='Name']]")
  )
  if (!length(nodes)) return(arg_empty_layers())

  bbox <- vapply(nodes, function(n) {
    geo <- xml2::xml_find_first(n, arg_xpath("EX_GeographicBoundingBox", ".//"))
    if (!is.na(geo)) {
      vals <- vapply(
        c("westBoundLongitude", "southBoundLatitude", "eastBoundLongitude", "northBoundLatitude"),
        function(tag) xml2::xml_text(xml2::xml_find_first(geo, arg_xpath(tag, "./"))),
        character(1)
      )
      return(paste(vals, collapse = ","))
    }
    ll <- xml2::xml_find_first(n, arg_xpath("LatLonBoundingBox", ".//"))
    if (is.na(ll)) return(NA_character_)
    paste(xml2::xml_attr(ll, c("minx", "miny", "maxx", "maxy")), collapse = ",")
  }, character(1))

  df <- data.frame(
    name     = arg_node_text(nodes, arg_xpath("Name", "./")),
    title    = arg_node_text(nodes, arg_xpath("Title", "./")),
    abstract = arg_node_text(nodes, arg_xpath("Abstract", "./")),
    crs      = arg_normalize_crs(arg_node_text(nodes, arg_xpath(c("CRS", "SRS"), "./"))),
    bbox     = bbox,
    stringsAsFactors = FALSE
  )
  arg_fill_titles(df)
}

#' Render a WMS layer as a raster
#'
#' Issues a `GetMap` request and returns the rendered image georeferenced as a
#' [terra::SpatRaster]. Unlike a WFS read this gives you the server's own
#' cartography — useful for basemaps, orthophotos and any layer published only
#' as a picture.
#'
#' @param x An organization name from [argentum_organizations()], or a WMS URL.
#' @param layer Layer name, or a character vector of names to be drawn in
#'   order, first at the bottom.
#' @param bbox Extent to render, as `c(xmin, ymin, xmax, ymax)` or any object
#'   accepted by [sf::st_bbox()]. Required: WMS has no "everything" request.
#' @param crs CRS of `bbox` and of the output, e.g. `"EPSG:4326"` or `4326`.
#'   Defaults to `"EPSG:4326"`, or to the CRS of `bbox` if it carries one.
#' @param width,height Image size in pixels. If only one is given, the other
#'   is derived from the aspect ratio of `bbox`. If neither is given, `width`
#'   defaults to 800.
#' @param style Optional named style, or one per layer.
#' @param format MIME type to request. `NULL` (default) negotiates one from the
#'   formats the service advertises for `GetMap`, preferring PNG, which keeps
#'   transparency. Pass a value such as `"image/jpeg"` to force it.
#' @param transparent Request a transparent background.
#' @param quiet Suppress progress messages.
#' @return A [terra::SpatRaster] with the CRS set, ready to combine with
#'   vector data read via [argentum_read_wfs()].
#' @seealso [argentum_wms_legend()] for the matching legend graphic.
#' @export
#' @examples
#' \dontrun{
#' r <- argentum_read_wms(
#'   "https://wms.ign.gob.ar/geoserver/ows",
#'   layer = "ign:provincia",
#'   bbox  = c(-59, -35, -57, -34),
#'   width = 800
#' )
#' terra::plotRGB(r)
#' }
argentum_read_wms <- function(x,
                              layer,
                              bbox,
                              crs = NULL,
                              width = NULL,
                              height = NULL,
                              style = NULL,
                              format = NULL,
                              transparent = TRUE,
                              quiet = arg_opt("argentum.quiet")) {
  if (missing(bbox) || is.null(bbox)) {
    cli::cli_abort(c(
      "{.arg bbox} is required.",
      "i" = "WMS renders a fixed extent; there is no request for a whole layer.",
      "i" = "Use the {.field bbox} column of {.fn argentum_layers} as a starting point."
    ))
  }

  crs <- crs %||% arg_bbox_crs(bbox) %||% "EPSG:4326"
  crs_str <- arg_crs_string(crs)
  box <- arg_bbox_numeric(bbox)

  dims <- arg_image_dims(box, width, height)

  cap <- argentum_capabilities(x, service = "wms")
  base <- attr(cap, "url")
  version <- attr(cap, "version")

  # Negotiate the image format against what the service advertises, rather
  # than asking for PNG and hoping. Some provincial MapServer installs publish
  # only image/gif or image/png8.
  format <- arg_choose_format(arg_wms_formats(cap), preferred = format)

  # A GetMap in a CRS the layer does not publish comes back as an exception, or
  # worse, as a blank tile. Checking here means the message names the CRSs that
  # would work. The CRS is not silently swapped: `bbox` is expressed in it, so
  # changing it would move the map.
  arg_check_wms_crs(cap, layer, crs_str)

  # WMS 1.3.0 renamed SRS to CRS and, for geographic CRSs, flipped axis order.
  is_130 <- arg_version_gte(version, "1.3.0")
  bbox_out <- if (is_130 && arg_is_latlon(crs_str)) box[c(2, 1, 4, 3)] else box

  query <- list(
    service     = "WMS",
    version     = version,
    request     = "GetMap",
    layers      = paste(layer, collapse = ","),
    styles      = paste(style %||% "", collapse = ","),
    format      = format,
    transparent = if (transparent) "TRUE" else "FALSE",
    width       = as.character(dims$width),
    height      = as.character(dims$height),
    bbox        = paste(format(bbox_out, scientific = FALSE), collapse = ",")
  )
  query[[if (is_130) "crs" else "srs"]] <- crs_str

  if (!quiet) cli::cli_progress_step("Rendering {.field {paste(layer, collapse = ', ')}} at {dims$width}x{dims$height}")

  resp <- arg_perform(arg_request(base, query), "GetMap request")
  arg_reject_xml(resp)

  ext <- switch(format, "image/png" = ".png", "image/jpeg" = ".jpg", "image/tiff" = ".tif")
  tmp <- tempfile("argentum_wms_", fileext = ext)
  writeBin(httr2::resp_body_raw(resp), tmp)

  rast <- terra::rast(tmp)
  terra::ext(rast) <- terra::ext(box[1], box[3], box[2], box[4])
  terra::crs(rast) <- crs_str
  if (!quiet) cli::cli_alert_success("Rendered {terra::nlyr(rast)} band{?s}.")
  rast
}

#' Retrieve the legend graphic for a WMS layer
#'
#' @inheritParams argentum_read_wms
#' @param layer A single layer name.
#' @return A [terra::SpatRaster] holding the legend image (no CRS: it is a
#'   picture, not a map).
#' @export
#' @examples
#' \dontrun{
#' argentum_wms_legend("https://wms.ign.gob.ar/geoserver/ows", "ign:provincia")
#' }
argentum_wms_legend <- function(x, layer, style = NULL, format = "image/png") {
  if (!rlang::is_string(layer)) cli::cli_abort("{.arg layer} must be a single string.")
  cap <- argentum_capabilities(x, service = "wms")
  format <- format %||% "image/png"

  resp <- arg_perform(
    arg_request(attr(cap, "url"), list(
      service = "WMS",
      version = attr(cap, "version"),
      request = "GetLegendGraphic",
      layer   = layer,
      style   = style,
      format  = format
    )),
    "GetLegendGraphic request"
  )
  arg_reject_xml(resp)

  tmp <- tempfile("argentum_legend_", fileext = ".png")
  writeBin(httr2::resp_body_raw(resp), tmp)
  terra::rast(tmp)
}

# --- helpers -------------------------------------------------------------

#' Refuse a CRS the layer does not publish, and say which ones it does
#' @noRd
arg_check_wms_crs <- function(cap, layer, crs_str) {
  available <- unique(unlist(lapply(layer, function(one) {
    arg_layer_crs_list(cap, one, service = "wms")
  })))
  # A service that declares nothing is not evidence that the CRS is wrong.
  if (!length(available)) return(invisible(NULL))
  if (toupper(crs_str) %in% toupper(available)) return(invisible(NULL))

  cli::cli_abort(c(
    "{.val {crs_str}} is not published for {.field {paste(layer, collapse = ', ')}}.",
    "i" = "The service offers {.val {utils::head(available, 8)}}.",
    "i" = "Pass {.arg crs} with one of those, and give {.arg bbox} in the same CRS."
  ), class = "argentum_error_crs")
}

# A GetMap that fails often returns XML with status 200. Catch it before
# handing a text file to terra.
#' @noRd
arg_reject_xml <- function(resp) {
  type <- httr2::resp_content_type(resp)
  if (!grepl("^image/", type)) {
    body <- try(arg_read_xml(httr2::resp_body_string(resp)), silent = TRUE)
    if (!inherits(body, "try-error")) {
      xml2::xml_ns_strip(body)
      arg_check_exception(body)
    }
    cli::cli_abort(
      "Expected an image but the service returned {.val {type}}.",
      class = "argentum_error_not_image"
    )
  }
  invisible(resp)
}

#' @noRd
arg_bbox_numeric <- function(bbox) {
  if (!is.numeric(bbox)) bbox <- as.numeric(sf::st_bbox(bbox))
  if (length(bbox) != 4L) cli::cli_abort("{.arg bbox} must have four values: xmin, ymin, xmax, ymax.")
  unname(bbox)
}

#' @noRd
arg_bbox_crs <- function(bbox) {
  if (is.numeric(bbox)) return(NULL)
  crs <- try(sf::st_crs(bbox), silent = TRUE)
  if (inherits(crs, "try-error") || is.na(crs$epsg)) return(NULL)
  paste0("EPSG:", crs$epsg)
}

#' @noRd
arg_image_dims <- function(box, width, height) {
  ratio <- (box[4] - box[2]) / (box[3] - box[1])
  if (is.null(width) && is.null(height)) width <- 800
  if (is.null(height)) height <- round(width * ratio)
  if (is.null(width)) width <- round(height / ratio)
  list(width = max(1L, as.integer(width)), height = max(1L, as.integer(height)))
}

#' @noRd
arg_is_latlon <- function(crs) {
  # WMS 1.3.0 honours the authority axis order for these; CRS:84 is
  # deliberately excluded because it is defined as lon/lat.
  crs %in% c("EPSG:4326", "EPSG:4258", "EPSG:4269", "EPSG:22171", "EPSG:22172",
             "EPSG:22173", "EPSG:22174", "EPSG:22175")
}
