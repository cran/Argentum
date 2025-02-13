## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# library(Argentum)
# library(sf)
# 
# # Get organization data
# org <- argentum_select_organization(search = "Buenos Aires")
# 
# # List available layers
# layers <- argentum_list_layers(org$Name)
# 
# # Import a specific layer
# sf_layer <- argentum_import_wfs_layer(org$WFS_URL, layers$Name[1])
# 
# # Now you can work with the data using sf functions
# st_crs(sf_layer)  # Check the coordinate reference system
# plot(sf_layer)    # Basic plot of the geometry

## ----eval=FALSE---------------------------------------------------------------
# # Get capabilities document
# capabilities <- argentum_get_capabilities(org$WFS_URL)
# 
# # The capabilities document contains information about:
# # - Available layers
# # - Supported operations
# # - Coordinate reference systems
# # - Output formats

## ----eval=FALSE---------------------------------------------------------------
# tryCatch({
#   # Attempt to import data
#   sf_layer <- argentum_import_wfs_layer(org$WFS_URL, layers$Name[1])
# }, error = function(e) {
#   # Handle any errors that occur
#   message("Error importing layer: ", e$message)
# })

## ----eval=FALSE---------------------------------------------------------------
# # Use appropriate timeout values for large datasets
# capabilities <- argentum_get_capabilities(
#   url = org$WFS_URL,
#   timeout = 60  # Increase timeout for slow connections
# )

## ----eval=FALSE---------------------------------------------------------------
# library(sf)
# library(dplyr)
# 
# # Check the data structure
# str(sf_layer)
# 
# # Basic statistics
# summary(sf_layer)
# 
# # Spatial operations
# sf_layer_transformed <- st_transform(sf_layer, 4326)
# 
# # Calculate areas if working with polygons
# if (all(st_geometry_type(sf_layer) %in% c("POLYGON", "MULTIPOLYGON"))) {
#   sf_layer$area <- st_area(sf_layer)
# }

## ----eval=FALSE---------------------------------------------------------------
# # Example of constructing a custom WFS URL
# base_url <- org$WFS_URL
# query_params <- list(
#   service = "WFS",
#   version = "1.1.0",
#   request = "GetFeature",
#   typeName = layers$Name[1],
#   outputFormat = "application/json"
# )
# 
# # Build the URL
# query_url <- httr::modify_url(
#   url = base_url,
#   query = query_params
# )

## -----------------------------------------------------------------------------
sessionInfo()

