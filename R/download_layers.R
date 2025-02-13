#' Download WFS Layers to Folder
#'
#' @name argentum_download_layers
#' @title Download WFS Layers to Folder
#' @description This function downloads WFS layers from specified organizations to a local folder.
#' It can download all layers from an organization or specific layers by name.
#'
#' @param organization A character string specifying the name of the organization
#' @param output_dir Path to the directory where files will be saved. Defaults to a temporary directory.
#' @param layer_names Optional vector of layer names to download. If NULL, downloads all available layers
#' @param format Output format for the files. One of "gpkg" (GeoPackage), "shp" (Shapefile),
#'              or "geojson" (GeoJSON)
#' @param overwrite Logical; should existing files be overwritten?
#' @return Invisibly returns a data frame with download results
#' @export
#'
#' @examples
#' \donttest{
#' # Get available organizations
#' orgs <- argentum_list_organizations()
#' if (nrow(orgs) > 0) {
#'   argentum_download_layers(orgs$Name[1], output_dir = tempdir())
#' }
#' }
argentum_download_layers <- function(organization,
                                     output_dir = file.path(tempdir(), "wfs_layers"),
                                     layer_names = NULL,
                                     format = c("gpkg", "shp", "geojson"),
                                     overwrite = FALSE) {
  
  # Validate and match format argument
  format <- match.arg(format)
  
  # Prevent using "data" as output directory
  if (tolower(output_dir) == "data") {
    stop("Using 'data' as output_dir is not allowed. Please choose another directory.")
  }
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message(sprintf("Created output directory: %s", output_dir))
  }
  
  # Get organization data
  orgs <- argentum_list_organizations()
  org_data <- orgs[orgs$Name == organization, ]
  
  if (nrow(org_data) == 0) {
    stop("Organization not found.")
  }
  
  # Get available layers
  layers <- argentum_list_layers(organization)
  
  if (nrow(layers) == 0) {
    stop("No layers found for the specified organization.")
  }
  
  # Filter layers if specific names are provided
  if (!is.null(layer_names)) {
    layers <- layers[layers$Name %in% layer_names, ]
    if (nrow(layers) == 0) {
      stop("None of the specified layers were found.")
    }
  }
  
  # Initialize results data frame
  results <- data.frame(
    layer_name = character(0),
    status = character(0),
    file_path = character(0),
    error_message = character(0),
    stringsAsFactors = FALSE
  )
  
  # Process each layer
  for (i in seq_len(nrow(layers))) {
    layer_name <- layers$Name[i]
    safe_name <- make.names(layer_name)
    file_name <- sprintf("%s.%s", safe_name, format)
    file_path <- file.path(output_dir, file_name)
    
    result <- data.frame(
      layer_name = layer_name,
      status = "pending",
      file_path = file_path,
      error_message = NA_character_,
      stringsAsFactors = FALSE
    )
    
    # Check if file exists and handle overwrite
    if (file.exists(file_path) && !overwrite) {
      result$status <- "skipped"
      result$error_message <- "File exists and overwrite=FALSE"
      results <- rbind(results, result)
      message(sprintf("Skipping %s: file already exists", layer_name))
      next
    }
    
    tryCatch({
      # Import the layer
      message(sprintf("Downloading layer: %s", layer_name))
      sf_layer <- argentum_import_wfs_layer(org_data$WFS_URL, layer_name)
      
      # Save the layer based on format
      switch(format,
             "gpkg" = sf::write_sf(sf_layer, file_path, driver = "GPKG"),
             "shp" = sf::write_sf(sf_layer, file_path, driver = "ESRI Shapefile"),
             "geojson" = sf::write_sf(sf_layer, file_path, driver = "GeoJSON")
      )
      
      result$status <- "success"
      message(sprintf("Successfully downloaded %s to %s", layer_name, file_path))
      
    }, error = function(e) {
      result$status <- "error"
      result$error_message <- conditionMessage(e)
      message(sprintf("Error downloading %s: %s", layer_name, conditionMessage(e)))
    })
    
    results <- rbind(results, result)
  }
  
  # Print summary
  success_count <- sum(results$status == "success")
  error_count <- sum(results$status == "error")
  skip_count <- sum(results$status == "skipped")
  
  message(sprintf("\nDownload Summary:"))
  message(sprintf("- Total layers processed: %d", nrow(results)))
  message(sprintf("- Successfully downloaded: %d", success_count))
  message(sprintf("- Errors: %d", error_count))
  message(sprintf("- Skipped: %d", skip_count))
  
  # Return results invisibly
  invisible(results)
}
