# Backward compatibility -------------------------------------------------
# Every 1.x exported function keeps working and points at its replacement.
# These are soft-deprecated: they warn once per session, not once per call.

#' Deprecated functions from argentum 1.x
#'
#' These wrappers keep 1.x scripts running. Each forwards to its 2.0
#' replacement and issues a deprecation warning. They will be removed in 3.0.
#'
#' @param organization,wfs_url,layer_name,layer_names,url Legacy arguments.
#' @param output_dir,format,overwrite Legacy arguments.
#' @param search,interactive_select Legacy arguments.
#' @param max_tries,timeout Legacy arguments, now package options.
#' @return The value of the replacement function.
#' @name argentum-deprecated
#' @keywords internal
NULL

#' @rdname argentum-deprecated
#' @export
argentum_list_organizations <- function() {
  arg_deprecate("argentum_list_organizations()", "argentum_organizations()")
  orgs <- argentum_organizations()
  # 1.x column names, for scripts that index by name.
  data.frame(
    Name = orgs$name, WMS_URL = orgs$wms_url, WFS_URL = orgs$wfs_url,
    stringsAsFactors = FALSE
  )
}

#' @rdname argentum-deprecated
#' @export
argentum_list_layers <- function(organization) {
  arg_deprecate("argentum_list_layers()", "argentum_layers()")
  layers <- argentum_layers(organization, service = "wfs")
  data.frame(Name = layers$name, Title = layers$title, stringsAsFactors = FALSE)
}

#' @rdname argentum-deprecated
#' @export
argentum_get_capabilities <- function(url, max_tries = 3, timeout = 30) {
  arg_deprecate("argentum_get_capabilities()", "argentum_capabilities()")
  withr_opts <- options(argentum.max_tries = max_tries, argentum.timeout = timeout)
  on.exit(options(withr_opts), add = TRUE)
  service <- if (grepl("service=wms", url, ignore.case = TRUE)) "wms" else "wfs"
  argentum_capabilities(url, service = service)
}

#' @rdname argentum-deprecated
#' @export
argentum_import_wfs_layer <- function(wfs_url, layer_name) {
  arg_deprecate("argentum_import_wfs_layer()", "argentum_read_wfs()")
  argentum_read_wfs(wfs_url, layer_name)
}

#' @rdname argentum-deprecated
#' @export
argentum_download_layers <- function(organization,
                                     output_dir = file.path(tempdir(), "wfs_layers"),
                                     layer_names = NULL,
                                     format = c("gpkg", "shp", "geojson"),
                                     overwrite = FALSE) {
  arg_deprecate("argentum_download_layers()", "argentum_download()")
  argentum_download(
    organization, layers = layer_names, dir = output_dir,
    format = match.arg(format), overwrite = overwrite
  )
}

#' @rdname argentum-deprecated
#' @export
argentum_interactive_import <- function() {
  arg_deprecate("argentum_interactive_import()", "argentum_browse()")
  argentum_browse(action = "read")
}

#' @rdname argentum-deprecated
#' @export
argentum_interactive_download <- function(output_dir = NULL) {
  arg_deprecate("argentum_interactive_download()", "argentum_browse()")
  argentum_browse(action = "download", dir = output_dir)
}

#' @rdname argentum-deprecated
#' @export
argentum_select_organization <- function(search = NULL, interactive_select = interactive()) {
  arg_deprecate("argentum_select_organization()", "argentum_search_organizations()")
  orgs <- if (is.null(search)) argentum_organizations() else argentum_search_organizations(search)
  if (nrow(orgs) == 0L) return(NULL)
  if (!interactive_select) return(orgs[1, , drop = FALSE])
  pick <- utils::menu(orgs$name, title = "Select an organization:")
  if (pick == 0L) NULL else orgs[pick, , drop = FALSE]
}

.arg_warned <- new.env(parent = emptyenv())

#' @noRd
arg_deprecate <- function(old, new) {
  if (isTRUE(.arg_warned[[old]])) return(invisible())
  .arg_warned[[old]] <- TRUE
  cli::cli_warn(
    c("{.fn {old}} was deprecated in argentum 2.0.0.",
      "i" = "Use {.fn {new}} instead.",
      "i" = "See {.file vignette(\"migrating-to-2-0\")}."),
    class = "argentum_deprecated"
  )
}
