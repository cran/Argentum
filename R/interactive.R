# Interactive browser ----------------------------------------------------

#' Browse services interactively
#'
#' A guided prompt that walks from organization to service to layer, then
#' either returns the data or writes it to disk. Intended for exploration at
#' the console; every step it performs is available as a normal function call.
#'
#' @param action `"read"` returns the object, `"download"` writes files.
#' @param dir Destination directory when `action = "download"`.
#' @return An `sf` object, a [terra::SpatRaster], or a download report,
#'   depending on the path taken. `NULL` if the user cancels.
#' @export
#' @examples
#' if (interactive()) {
#'   layer <- argentum_browse()
#' }
argentum_browse <- function(action = c("read", "download"), dir = NULL) {
  action <- match.arg(action)
  if (!interactive()) {
    cli::cli_abort("{.fn argentum_browse} needs an interactive session.")
  }

  orgs <- argentum_organizations()
  pick <- utils::menu(orgs$name, title = "Select an organization (0 to cancel):")
  if (pick == 0L) return(invisible(NULL))
  org <- orgs[pick, , drop = FALSE]

  services <- c(if (nzchar(org$wfs_url)) "wfs", if (nzchar(org$wms_url)) "wms")
  service <- if (length(services) == 1L) services else {
    s <- utils::menu(toupper(services), title = "Which service?")
    if (s == 0L) return(invisible(NULL)) else services[s]
  }

  layers <- argentum_layers(org$name, service = service)
  if (nrow(layers) == 0L) {
    cli::cli_alert_warning("No layers published.")
    return(invisible(NULL))
  }

  labels <- paste0(layers$name, "  (", layers$title, ")")
  choices <- if (service == "wfs" && action == "download") c("ALL LAYERS", labels) else labels
  pick <- utils::menu(choices, title = "Select a layer (0 to cancel):")
  if (pick == 0L) return(invisible(NULL))

  if (service == "wms") {
    box <- arg_prompt_bbox(layers$bbox[pick])
    return(argentum_read_wms(org$name, layers$name[pick], bbox = box))
  }

  if (action == "read") {
    return(argentum_read_wfs(org$name, layers$name[pick]))
  }

  selected <- if (identical(choices[pick], "ALL LAYERS")) NULL else layers$name[pick - 1L]
  fmt <- c("gpkg", "geojson", "shp")
  f <- utils::menu(fmt, title = "Output format:")
  if (f == 0L) return(invisible(NULL))

  dir <- dir %||% readline("Output directory (Enter for a temporary folder): ")
  if (!nzchar(dir)) dir <- file.path(tempdir(), "argentum")

  argentum_download(org$name, layers = selected, dir = dir, format = fmt[f])
}

#' @noRd
arg_prompt_bbox <- function(default) {
  if (!is.na(default) && nzchar(default)) {
    cli::cli_text("Layer extent: {.val {default}}")
    ans <- readline("Bounding box as xmin,ymin,xmax,ymax (Enter to use the full extent): ")
    if (!nzchar(ans)) ans <- default
  } else {
    ans <- readline("Bounding box as xmin,ymin,xmax,ymax: ")
  }
  box <- suppressWarnings(as.numeric(strsplit(ans, "[, ]+")[[1]]))
  if (length(box) != 4L || anyNA(box)) cli::cli_abort("Could not parse the bounding box.")
  box
}
