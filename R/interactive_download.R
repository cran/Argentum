#' Download WFS Layers Interactively
#'
#' @name argentum_interactive_download
#' @title Download WFS Layers Interactively
#' @description This function provides an interactive interface to download WFS layers.
#' Users can select an organization, choose specific layers or download all,
#' and specify the output format and directory.
#'
#' @param output_dir Optional; Path to the directory where files will be saved. If NULL, user will be prompted
#' @return Invisibly returns a data frame with download results
#' @export
#'
#' @examples
#' \donttest{
#' # Try to download interactively
#' tryCatch({
#'   results <- argentum_interactive_download()
#' }, error = function(e) {
#'   message("Error: ", e$message)
#' })
#' }
argentum_interactive_download <- function(output_dir = NULL) {
  # Step 1: Select organization
  orgs <- argentum_list_organizations()
  cat("\nAvailable organizations:\n")
  for (i in seq_len(nrow(orgs))) {
    cat(i, ": ", orgs$Name[i], "\n")
  }

  org_choice <- as.integer(readline(prompt = "\nEnter the number of the organization you want to select: "))
  if (is.na(org_choice) || org_choice < 1 || org_choice > nrow(orgs)) {
    stop("Invalid organization selection")  # Note: removed the period for exact error matching
  }
  selected_org <- orgs[org_choice, ]
  message(sprintf("\nSelected organization: %s", selected_org$Name))

  # Check if the selected organization has a valid WFS URL
  if (is.na(selected_org$WFS_URL) || selected_org$WFS_URL == "") {
    stop("The selected organization does not have a valid WFS URL")
  }

  # Step 2: List layers
  message("\nRetrieving WFS layers...")
  layers <- argentum_list_layers(selected_org$Name)
  if (nrow(layers) == 0) {
    stop("No WFS layers available for this organization")
  }

  cat("\nAvailable WFS layers:\n")
  cat("0: Download ALL layers\n")  # Option to download all layers
  for (i in seq_len(nrow(layers))) {
    cat(i, ": ", layers$Name[i], " (", layers$Title[i], ")\n")
  }

  # Step 3: Select layers
  layer_choice <- as.integer(readline(prompt = "\nEnter the number of the WFS layer you want to download (0 for all): "))
  if (is.na(layer_choice) || layer_choice < 0 || layer_choice > nrow(layers)) {
    stop("Invalid layer selection")  # Note: removed the period for exact error matching
  }

  selected_layers <- if (layer_choice == 0) {
    layers$Name
  } else {
    layers$Name[layer_choice]
  }

  # Step 4: Select format
  cat("\nAvailable formats:\n")
  formats <- c("gpkg", "shp", "geojson")
  for (i in seq_along(formats)) {
    cat(i, ": ", formats[i], "\n")
  }

  format_choice <- as.integer(readline(prompt = "\nEnter the number of the format you want to use: "))
  if (is.na(format_choice) || format_choice < 1 || format_choice > length(formats)) {
    stop("Invalid format selection")
  }
  selected_format <- formats[format_choice]

  # Step 5: Get output directory
  if (is.null(output_dir)) {
    output_dir <- readline(prompt = "\nEnter the output directory path (press Enter for 'wfs_layers'): ")
    if (output_dir == "") output_dir <- "wfs_layers"
  }

  # Step 6: Confirm overwrite
  overwrite <- FALSE
  if (dir.exists(output_dir) && length(list.files(output_dir)) > 0) {
    answer <- readline(prompt = "\nOutput directory contains files. Do you want to overwrite existing files? (y/N): ")
    overwrite <- tolower(substr(answer, 1, 1)) == "y"
  }

  # Step 7: Download layers
  message("\nStarting download...")
  tryCatch({
    results <- argentum_download_layers(
      organization = selected_org$Name,
      output_dir = output_dir,
      layer_names = selected_layers,
      format = selected_format,
      overwrite = overwrite
    )

    # Return results invisibly
    invisible(results)
  }, error = function(e) {
    # Re-throw the error to ensure proper error handling in tests
    stop(e$message)
  })
}
