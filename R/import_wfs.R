#' Import WFS Layer
#'
#' @param wfs_url The URL of the WFS service
#' @param layer_name The name of the layer to import
#' @return An sf object
#' @export
#'
#' @examples
#' \donttest{
#' tryCatch({
#'   sf_layer <- argentum_import_wfs_layer("http://example.com/wfs", "example_layer")
#'   print(sf_layer)
#' }, error = function(e) {
#'   message("Error occurred: ", e$message)
#' })
#' }
argentum_import_wfs_layer <- function(wfs_url, layer_name) {
  # Parse the existing URL
  parsed_url <- httr::parse_url(wfs_url)

  # Remove existing query parameters
  parsed_url$query <- list()

  # Add our parameters
  parsed_url$query$service <- "WFS"
  parsed_url$query$version <- "1.1.0"  # Use the version from the original URL
  parsed_url$query$request <- "GetFeature"
  parsed_url$query$typeName <- layer_name
  parsed_url$query$outputFormat <- "application/json"

  # Rebuild the URL
  query_url <- httr::build_url(parsed_url)

  message(sprintf("Requesting WFS feature from: %s", query_url))

  tryCatch({
    sf_layer <- sf::read_sf(query_url)
    return(sf_layer)
  }, error = function(e) {
    stop(sprintf("Error importing WFS layer: %s", e$message))
  })
}
