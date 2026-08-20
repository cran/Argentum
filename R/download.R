# Bulk download ----------------------------------------------------------

#' Download WFS layers to disk
#'
#' Writes one file per layer and returns a report. Failures are recorded and
#' the run continues, so one broken layer does not abort a bulk download.
#'
#' @inheritParams argentum_read_wfs
#' @param layers Character vector of layer names. `NULL` (default) downloads
#'   every layer the service publishes.
#' @param dir Destination directory. Created if absent. Defaults to a session
#'   temporary directory, per CRAN policy on writing to the user's filespace.
#' @param format One of `"gpkg"` (default), `"geojson"` or `"shp"`.
#'   GeoPackage is recommended: Shapefile truncates field names to ten
#'   characters and cannot hold more than 2 GB.
#' @param overwrite Overwrite files that already exist.
#' @return Invisibly, a data frame with one row per layer and columns `layer`,
#'   `status` (`"success"`, `"skipped"` or `"error"`), `path`, `features` and
#'   `message`.
#' @export
#' @examples
#' \dontrun{
#' report <- argentum_download(
#'   "https://wms.ign.gob.ar/geoserver/ows",
#'   layers = "ign:provincia",
#'   dir = tempdir()
#' )
#' subset(report, status == "error")
#' }
argentum_download <- function(x,
                              layers = NULL,
                              dir = file.path(tempdir(), "argentum"),
                              format = c("gpkg", "geojson", "shp"),
                              overwrite = FALSE,
                              bbox = NULL,
                              crs = NULL,
                              max_features = NULL,
                              quiet = arg_opt("argentum.quiet")) {
  format <- match.arg(format)

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    arg_inform("Created {.path {dir}}")
  }
  if (format == "shp") {
    cli::cli_warn(c(
      "Shapefile truncates field names to 10 characters and loses some types.",
      "i" = "Prefer {.val gpkg} unless a downstream tool requires {.val shp}."
    ))
  }

  available <- argentum_layers(x, service = "wfs")
  if (nrow(available) == 0L) {
    cli::cli_abort("The service publishes no WFS layers.", class = "argentum_error_no_layers")
  }

  if (!is.null(layers)) {
    unknown <- setdiff(layers, available$name)
    if (length(unknown) == length(layers)) {
      cli::cli_abort(
        "None of the requested layer{?s} exist{?s} on this service: {.val {unknown}}.",
        class = "argentum_error_no_layers"
      )
    }
    if (length(unknown)) cli::cli_warn("Skipping unknown layer{?s}: {.val {unknown}}.")
    available <- available[available$name %in% layers, , drop = FALSE]
  }

  # Pre-allocate: v1 grew the report with rbind() inside the loop.
  n <- nrow(available)
  report <- data.frame(
    layer    = available$name,
    status   = rep("pending", n),
    path     = NA_character_,
    features = NA_integer_,
    message  = NA_character_,
    stringsAsFactors = FALSE
  )

  for (i in seq_len(n)) {
    name <- available$name[i]
    file <- file.path(dir, paste0(arg_safe_filename(name), ".", format))
    report$path[i] <- file

    if (file.exists(file) && !overwrite) {
      report$status[i] <- "skipped"
      report$message[i] <- "File exists and overwrite = FALSE"
      arg_inform("Skipping {.field {name}}: file already exists.")
      next
    }

    # NOTE: the result is assigned in this frame, not inside the handler.
    # v1 assigned inside the error handler, where it modified a copy, so
    # every failure was silently reported as "pending".
    outcome <- rlang::try_fetch({
      layer_sf <- argentum_read_wfs(
        x, name, bbox = bbox, crs = crs,
        max_features = max_features, quiet = TRUE
      )
      sf::write_sf(layer_sf, file, driver = arg_driver(format), delete_dsn = overwrite)
      list(status = "success", features = nrow(layer_sf), message = NA_character_)
    }, error = function(cnd) {
      list(status = "error", features = NA_integer_, message = conditionMessage(cnd))
    })

    report$status[i]   <- outcome$status
    report$features[i] <- outcome$features
    report$message[i]  <- outcome$message

    if (!quiet) {
      if (outcome$status == "success") {
        cli::cli_alert_success("{.field {name}}: {outcome$features} feature{?s}")
      } else {
        cli::cli_alert_danger("{.field {name}}: {outcome$message}")
      }
    }
  }

  if (!quiet) arg_report_summary(report, dir)
  invisible(report)
}

#' @noRd
arg_report_summary <- function(report, dir) {
  tally <- table(factor(report$status, levels = c("success", "skipped", "error")))
  cli::cli_h3("Download summary")
  cli::cli_bullets(c(
    "v" = "{tally[['success']]} succeeded",
    "i" = "{tally[['skipped']]} skipped",
    "x" = "{tally[['error']]} failed"
  ))
  cli::cli_text("Files in {.path {dir}}")
}

#' @noRd
arg_driver <- function(format) {
  switch(format, gpkg = "GPKG", geojson = "GeoJSON", shp = "ESRI Shapefile")
}

# Layer names look like "ign:provincia" or "capa con espacios". Produce
# something portable without make.names(), which leaves dots everywhere.
#' @noRd
arg_safe_filename <- function(x) {
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  substr(gsub("^_|_$", "", x), 1, 100)
}
