## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = FALSE)

## ----setup--------------------------------------------------------------------
# library(Argentum)
# library(terra)
# library(sf)

## -----------------------------------------------------------------------------
# ign <- "https://wms.ign.gob.ar/geoserver/ows"
# 
# r <- argentum_read_wms(
#   ign,
#   layer = "ign:provincia",
#   bbox  = c(-59, -35, -57, -34),
#   width = 1000
# )
# 
# r

## -----------------------------------------------------------------------------
# layers <- argentum_layers(ign, service = "wms")
# layers[layers$name == "ign:provincia", "bbox"]

## -----------------------------------------------------------------------------
# aoi <- c(-59, -35, -57, -34)
# 
# basemap  <- argentum_read_wms(ign, "ign:provincia", bbox = aoi, width = 1200)
# boundaries <- argentum_read_wfs(ign, "ign:provincia", bbox = aoi, crs = 4326)
# 
# terra::plotRGB(basemap)
# plot(sf::st_geometry(boundaries), add = TRUE, border = "white", lwd = 2)

## -----------------------------------------------------------------------------
# argentum_read_wms(
#   ign,
#   layer = c("ign:provincia", "ign:limite_politico"),
#   bbox  = aoi
# )

## -----------------------------------------------------------------------------
# legend <- argentum_wms_legend(ign, "ign:provincia")
# terra::plotRGB(legend)

## -----------------------------------------------------------------------------
# argentum_read_wms(ign, "some_orthophoto", bbox = aoi, format = "image/jpeg",
#                   transparent = FALSE)

## -----------------------------------------------------------------------------
# argentum_capabilities(ign, "wms", version = "1.1.1")

