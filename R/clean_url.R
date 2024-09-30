#' Clean and format URL
#'
#' @param url The URL to clean
#' @return A cleaned and formatted URL
#' @keywords internal
clean_url <- function(url) {
  url <- trimws(url)
  url <- gsub("\\s*\\([^\\)]+\\)\\s*$", "", url)
  return(url)
}
