## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = FALSE)

## ----setup--------------------------------------------------------------------
# library(Argentum)

## -----------------------------------------------------------------------------
# orgs <- argentum_organizations()
# nrow(orgs)
# head(orgs$name)

## -----------------------------------------------------------------------------
# argentum_search_organizations("catastro")
# argentum_search_organizations("buenos aires", service = "wms")

## -----------------------------------------------------------------------------
# ign <- "https://wms.ign.gob.ar/geoserver/ows"

## -----------------------------------------------------------------------------
# layers <- argentum_layers(ign)
# layers[, c("name", "title", "crs")]

## -----------------------------------------------------------------------------
# provinces <- argentum_read_wfs(ign, "ign:provincia")

## -----------------------------------------------------------------------------
# argentum_read_wfs(
#   ign, "ign:provincia",
#   bbox   = c(-59, -35, -57, -34),
#   crs    = 4326,
#   filter = "nam = 'Buenos Aires'"
# )

## -----------------------------------------------------------------------------
# aoi <- sf::read_sf("my_study_area.gpkg")
# argentum_read_wfs(ign, "ign:localidad", bbox = aoi)

## -----------------------------------------------------------------------------
# argentum_read_wfs(ign, "ign:localidad", page_size = Inf, max_features = 1000)

## -----------------------------------------------------------------------------
# report <- argentum_download(ign, dir = "data/ign", format = "gpkg")

## -----------------------------------------------------------------------------
# subset(report, status == "error")[, c("layer", "message")]

## -----------------------------------------------------------------------------
# result <- tryCatch(
#   argentum_read_wfs(ign, "ign:provincia"),
#   argentum_error_offline = function(e) {
#     message("No network; using last night's extract instead.")
#     sf::read_sf("cache/provincias.gpkg")
#   }
# )

## -----------------------------------------------------------------------------
# argentum_cache_path()
# argentum_cache_clear()
# options(argentum.cache_ttl = 60 * 60 * 24 * 7)  # a week

## -----------------------------------------------------------------------------
# options(argentum.catalog = "endpoints.csv")

