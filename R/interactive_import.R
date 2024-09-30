#' Import WFS Layer Interactively
#'
#' @return An sf object
#' @export
#'
#' @examples
#' \donttest{
#' sf_layer <- argentum_interactive_import()
#' }
argentum_interactive_import <- function() {
  tryCatch({
    # Step 1: Select organization
    orgs <- argentum_list_organizations()
    cat("Available organizations:\n")
    for (i in seq_len(nrow(orgs))) {
      cat(i, ": ", orgs$Name[i], "\n")
    }
    org_choice <- as.integer(readline(prompt = "Enter the number of the organization you want to select: "))
    if (is.na(org_choice) || org_choice < 1 || org_choice > nrow(orgs)) {
      stop("Invalid organization selection.")
    }
    selected_org <- orgs[org_choice, ]
    message(sprintf("Selected organization: %s", selected_org$Name))

    # Check if the selected organization has a valid WFS URL
    if (is.na(selected_org$WFS_URL) || selected_org$WFS_URL == "") {
      stop("The selected organization does not have a valid WFS URL.")
    }

    # Step 2: List layers
    message("Attempting to retrieve WFS layers...")
    layers <- argentum_list_layers(selected_org$Name)
    if (nrow(layers) == 0) {
      stop("No WFS layers available for this organization.")
    }
    cat("\nAvailable WFS layers:\n")
    for (i in seq_len(nrow(layers))) {
      cat(i, ": ", layers$Name[i], " (", layers$Title[i], ")\n")
    }
    layer_choice <- as.integer(readline(prompt = "Enter the number of the WFS layer you want to import: "))
    if (is.na(layer_choice) || layer_choice < 1 || layer_choice > nrow(layers)) {
      stop("Invalid layer selection.")
    }
    selected_layer <- layers$Name[layer_choice]

    # Step 3: Import layer
    message("Attempting to import WFS layer...")
    imported_layer <- argentum_import_wfs_layer(selected_org$WFS_URL, selected_layer)
    message("WFS Layer imported successfully!")
    return(imported_layer)
  }, error = function(e) {
    message(sprintf("An error occurred: %s", conditionMessage(e)))
    message("This could be due to the server being down, an incorrect URL, or invalid user input.")
    message("Please try again or select a different organization.")
    return(NULL)
  })
}
