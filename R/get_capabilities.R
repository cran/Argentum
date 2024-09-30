#' Get WMS or WFS Capabilities
#'
#' @param url The full URL of the WMS or WFS service, including all parameters
#' @param max_tries The maximum number of attempts to connect
#' @param timeout The timeout for each attempt in seconds
#' @return An XML document containing the service capabilities
#' @export
#'
#' @examples
#' \donttest{
#' try({
#'   capabilities <- argentum_get_capabilities("http://example.com/wfs")
#'   print(capabilities)
#' })
#' }
argentum_get_capabilities <- function(url, max_tries = 3, timeout = 30) {
  message(sprintf("Requesting capabilities from: %s", url))
  for (i in 1:max_tries) {
    tryCatch({
      response <- httr::GET(url, httr::timeout(timeout))
      httr::stop_for_status(response)
      xml <- xml2::read_xml(httr::content(response, "text", encoding = "UTF-8"))
      # Remove all namespaces to simplify parsing
      xml2::xml_ns_strip(xml)
      return(xml)
    }, error = function(e) {
      if (i == max_tries) {
        stop(sprintf("Failed to retrieve capabilities after %d attempts: %s", max_tries, e$message))
      } else {
        message(sprintf("Attempt %d failed. Retrying...", i))
        Sys.sleep(2)  # Wait for 2 seconds before retrying
      }
    })
  }
}
