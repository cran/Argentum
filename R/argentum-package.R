#' argentum: Argentine geospatial web services
#'
#' Discovers and reads the geospatial layers published by Argentine public
#' organizations through the OGC standards WFS (vectors) and WMS (rendered
#' maps). Every exported function starts with `argentum_`; type that at the
#' console and press Tab to see the whole API.
#'
#' @section Finding data:
#' The usual entry point. Locate an organization, then see what it publishes.
#' \describe{
#'   \item{[argentum_organizations()]}{The catalogue of publishing
#'     organizations, as maintained by IDERA. One row per organization, with
#'     its WMS/WFS/WCS/CSW endpoints.}
#'   \item{[argentum_search_organizations()]}{Search that catalogue by
#'     case-insensitive regular expression.}
#'   \item{[argentum_layers()]}{The layers a service publishes, with title,
#'     abstract, CRS and bounding box.}
#'   \item{[argentum_capabilities()]}{The parsed `GetCapabilities` document,
#'     protocol version already negotiated. Rarely needed directly.}
#' }
#'
#' @section Reading vectors (WFS):
#' \describe{
#'   \item{[argentum_read_wfs()]}{Read a layer as an [sf::sf] data frame,
#'     with server-side `bbox`/CQL filtering, reprojection and automatic
#'     pagination.}
#'   \item{[argentum_download()]}{Write many layers straight to disk, with a
#'     per-layer status report; one failure does not abort the run.}
#' }
#'
#' @section Reading rasters (WMS):
#' \describe{
#'   \item{[argentum_read_wms()]}{Render a layer server-side and return it as
#'     a georeferenced [terra::SpatRaster].}
#'   \item{[argentum_wms_legend()]}{The matching legend graphic.}
#' }
#'
#' @section Interactive use, cache and options:
#' \describe{
#'   \item{[argentum_browse()]}{Pick an organization and layer from a menu.}
#'   \item{[argentum_help()]}{A guided console tour by topic, in Spanish or
#'     English, with copy-pasteable code.}
#'   \item{[argentum_cache_path()], [argentum_cache_clear()]}{Where the
#'     on-disk cache lives, and how to empty it.}
#'   \item{[argentum_options]}{Timeouts, retries, page size, cache TTL and
#'     catalogue override, all set through [options()].}
#' }
#'
#' @section Deprecated:
#' The 1.x API ([argentum_list_organizations()] and friends) still works with
#' a once-per-session warning and is removed in 3.0.0; see
#' `vignette("migrating-to-2-0")`.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang abort warn inform
## usethis namespace: end
NULL

.argentum_defaults <- list(
  argentum.timeout   = 30,
  argentum.max_tries = 3,
  argentum.page_size = 5000L,
  argentum.cache_ttl = 86400,
  argentum.catalog   = NULL,
  argentum.quiet     = FALSE
)

arg_opt <- function(name) {
  getOption(name, default = .argentum_defaults[[name]])
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(.argentum_defaults) %in% names(op))
  if (any(toset)) options(.argentum_defaults[toset])
  invisible()
}

#' Package options
#'
#' `Argentum` reads its defaults from [options()]. Set them in a script or in
#' `.Rprofile`.
#'
#' \describe{
#'   \item{`argentum.timeout`}{Seconds before an individual HTTP request is
#'     abandoned. Default `30`.}
#'   \item{`argentum.max_tries`}{How many times a transient failure is retried
#'     with exponential backoff. Default `3`.}
#'   \item{`argentum.page_size`}{Features requested per WFS page. Default
#'     `5000`. Lower it for servers that cap response size.}
#'   \item{`argentum.cache_ttl`}{Seconds a cached catalogue or capabilities
#'     document stays fresh. Default `86400` (one day).}
#'   \item{`argentum.catalog`}{Path to a CSV overriding the bundled endpoint
#'     catalogue. See `vignette("getting-started", package = "Argentum")`.}
#'   \item{`argentum.quiet`}{Suppress progress messages. Default `FALSE`.}
#' }
#'
#' @name argentum_options
#' @keywords internal
NULL
