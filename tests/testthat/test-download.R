test_that("filenames survive namespaced layer names", {
  # v1 used make.names(), which turned this into ign.provincia
  expect_equal(arg_safe_filename("ign:provincia"), "ign_provincia")
  expect_equal(arg_safe_filename("capa con espacios"), "capa_con_espacios")
  expect_equal(arg_safe_filename("a//b\\c"), "a_b_c")
  expect_equal(arg_safe_filename("__trim__"), "trim")
  expect_lte(nchar(arg_safe_filename(strrep("x", 300))), 100L)
})

test_that("formats map to the expected GDAL drivers", {
  expect_equal(arg_driver("gpkg"), "GPKG")
  expect_equal(arg_driver("geojson"), "GeoJSON")
  expect_equal(arg_driver("shp"), "ESRI Shapefile")
})

test_that("a download report records failures rather than leaving them pending", {
  # Regression test for the 1.x bug: the tryCatch handler assigned to a copy
  # of the result row, so errors were silently reported as "pending".
  local_quiet()
  dir <- withr::local_tempdir()

  testthat::local_mocked_bindings(
    argentum_layers = function(...) {
      data.frame(
        name = c("ok_layer", "broken_layer"),
        title = c("Fine", "Broken"),
        abstract = NA_character_, crs = NA_character_, bbox = NA_character_,
        stringsAsFactors = FALSE
      )
    },
    argentum_read_wfs = function(x, layer, ...) {
      if (layer == "broken_layer") stop("server exploded")
      sf::st_sf(
        id = 1L,
        geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
      )
    }
  )

  report <- argentum_download("https://x/ows", dir = dir)

  expect_equal(nrow(report), 2L)
  expect_equal(report$status, c("success", "error"))
  expect_false(any(report$status == "pending"))
  expect_match(report$message[2], "server exploded")
  expect_equal(report$features[1], 1L)
  expect_true(file.exists(report$path[1]))
})

test_that("existing files are skipped unless overwrite is TRUE", {
  local_quiet()
  dir <- withr::local_tempdir()
  file.create(file.path(dir, "ok_layer.gpkg"))

  testthat::local_mocked_bindings(
    argentum_layers = function(...) {
      data.frame(
        name = "ok_layer", title = "Fine", abstract = NA_character_,
        crs = NA_character_, bbox = NA_character_, stringsAsFactors = FALSE
      )
    },
    argentum_read_wfs = function(...) stop("should not be called")
  )

  report <- argentum_download("https://x/ows", dir = dir)
  expect_equal(report$status, "skipped")
})

test_that("requesting only unknown layers is an error, but a partial match warns", {
  local_quiet()
  dir <- withr::local_tempdir()

  testthat::local_mocked_bindings(
    argentum_layers = function(...) {
      data.frame(
        name = "real", title = "Real", abstract = NA_character_,
        crs = NA_character_, bbox = NA_character_, stringsAsFactors = FALSE
      )
    },
    argentum_read_wfs = function(...) {
      sf::st_sf(id = 1L, geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326))
    }
  )

  expect_error(
    argentum_download("https://x/ows", layers = "ghost", dir = dir),
    class = "argentum_error_no_layers"
  )
  expect_warning(
    argentum_download("https://x/ows", layers = c("real", "ghost"), dir = dir),
    "Skipping unknown"
  )
})
