#' List Layers for a Given Organization
#'
#' This function retrieves and lists the available layers for a specified organization.
#'
#' @param organization A character string specifying the name of the organization
#' @return A data frame containing layer information with columns 'Name' and 'Title'
#' @export
#'
#' @examples
#' \donttest{
#' # This example uses a mock organization name.
#' # In a real scenario, use an actual organization name from argentum_list_organizations()
#' tryCatch({
#'   orgs <- argentum_list_organizations()
#'   if(nrow(orgs) > 0) {
#'     layers <- argentum_list_layers(orgs$Name[1])
#'     print(layers)
#'   } else {
#'     message("No organizations found.")
#'   }
#' }, error = function(e) {
#'   message("Error occurred: ", e$message)
#' })
#' }
argentum_list_layers <- function(organization) {
  if (missing(organization) || !is.character(organization) || nchar(organization) == 0) {
    stop("Please provide a valid organization name.")
  }

  orgs <- argentum_list_organizations()
  org_data <- orgs[orgs$Name == organization, ]

  if (nrow(org_data) == 0) {
    stop("Organization not found.")
  }

  url <- org_data$WFS_URL

  # Check if the URL is for WMS
  if (grepl("service=wms", tolower(url))) {
    message("WMS URL detected. Attempting to construct WFS URL...")
    # Attempt to construct a WFS URL from the WMS URL
    url <- gsub("service=wms", "service=wfs", url, ignore.case = TRUE)
    url <- gsub("version=1.3.0", "version=1.1.0", url, ignore.case = TRUE)
  }

  # Ensure the URL is for WFS
  if (!grepl("service=wfs", tolower(url))) {
    url <- paste0(url, ifelse(grepl("\\?", url), "&", "?"), "service=WFS&version=1.1.0&request=GetCapabilities")
  }

  message(sprintf("Attempting to connect to WFS URL: %s", url))

  tryCatch({
    capabilities <- argentum_get_capabilities(url)

    layers <- xml2::xml_find_all(capabilities, "//FeatureType")
    layer_info <- lapply(layers, function(layer) {
      name <- xml2::xml_text(xml2::xml_find_first(layer, "./Name"))
      title <- xml2::xml_text(xml2::xml_find_first(layer, "./Title"))
      if (is.na(title)) title <- name
      data.frame(Name = name, Title = title, stringsAsFactors = FALSE)
    })

    result <- do.call(rbind, layer_info)

    if (is.null(result) || nrow(result) == 0) {
      warning("No WFS layers found for the specified organization.")
      return(data.frame(Name = character(0), Title = character(0), stringsAsFactors = FALSE))
    }

    return(result)
  }, error = function(e) {
    message(sprintf("Error occurred while retrieving WFS layers: %s", conditionMessage(e)))
    message("URL might be incorrect or the server might be down.")
    stop("Failed to retrieve WFS layers.")
  })
}
