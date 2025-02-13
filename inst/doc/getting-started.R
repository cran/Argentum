## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# # install.packages("devtools")
# devtools::install_github("your-username/Argentum")

## ----eval=FALSE---------------------------------------------------------------
# library(Argentum)
# 
# # Get list of organizations
# organizations <- argentum_list_organizations()
# print(organizations)

## ----eval=FALSE---------------------------------------------------------------
# # Interactive selection
# selected_org <- argentum_select_organization()
# 
# # Search for specific organizations
# buenos_aires_org <- argentum_select_organization(search = "Buenos Aires")

## ----eval=FALSE---------------------------------------------------------------
# # List available layers
# layers <- argentum_list_layers(selected_org$Name)
# print(layers)

## ----eval=FALSE---------------------------------------------------------------
# # Import a specific layer
# sf_layer <- argentum_import_wfs_layer(
#   wfs_url = selected_org$WFS_URL,
#   layer_name = layers$Name[1]
# )
# 
# # Basic plot of the imported data
# plot(sf_layer)

## ----eval=FALSE---------------------------------------------------------------
# sf_layer <- argentum_interactive_import()

## ----eval=FALSE---------------------------------------------------------------
# tryCatch({
#   sf_layer <- argentum_import_wfs_layer(
#     wfs_url = "http://example.com/wfs",
#     layer_name = "example_layer"
#   )
# }, error = function(e) {
#   message("Error occurred: ", e$message)
# })

## ----eval=FALSE---------------------------------------------------------------
# vignette("argentum", package = "Argentum")

