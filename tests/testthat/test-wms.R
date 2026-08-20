test_that("image dimensions preserve the bbox aspect ratio", {
  box <- c(-60, -36, -58, -34)   # 2 wide, 2 tall

  expect_equal(arg_image_dims(box, 800, NULL), list(width = 800L, height = 800L))
  expect_equal(arg_image_dims(box, NULL, 400), list(width = 400L, height = 400L))
  expect_equal(arg_image_dims(box, NULL, NULL)$width, 800L)

  wide <- c(-60, -35, -56, -34)  # 4 wide, 1 tall
  expect_equal(arg_image_dims(wide, 800, NULL), list(width = 800L, height = 200L))
})

test_that("both dimensions are honoured when both are supplied", {
  expect_equal(
    arg_image_dims(c(-60, -36, -58, -34), 300, 900),
    list(width = 300L, height = 900L)
  )
})

test_that("axis order is flipped only for latitude-first CRSs", {
  expect_true(arg_is_latlon("EPSG:4326"))
  # Argentine Gauss-Kruger zones are defined northing-first
  expect_true(arg_is_latlon("EPSG:22173"))
  # CRS:84 is deliberately lon/lat and must not be flipped
  expect_false(arg_is_latlon("CRS:84"))
  expect_false(arg_is_latlon("EPSG:3857"))
})

test_that("bbox is required, with an explanation", {
  expect_error(argentum_read_wms("https://x/ows", "layer"), "is required")
})

test_that("legend requires a single layer name", {
  expect_error(argentum_wms_legend("https://x/ows", c("a", "b")), "single string")
})

test_that("a CRS the layer does not publish is refused, with the ones that work", {
  cap <- as_capabilities(wms_capabilities_xml(), "WMS", "1.3.0")

  expect_silent(arg_check_wms_crs(cap, "capabaseargenmap", "EPSG:4326"))
  expect_silent(arg_check_wms_crs(cap, "capabaseargenmap", "EPSG:3857"))
  expect_error(
    arg_check_wms_crs(cap, "capabaseargenmap", "EPSG:22185"),
    class = "argentum_error_crs"
  )
  expect_error(
    arg_check_wms_crs(cap, "capabaseargenmap", "EPSG:22185"),
    "EPSG:4326"
  )
})

test_that("a service that declares no CRS is not second-guessed", {
  bare <- as_capabilities(
    xml2::read_xml("<WMS_Capabilities><Capability><Layer><Name>x</Name></Layer></Capability></WMS_Capabilities>"),
    "WMS", "1.3.0"
  )
  expect_silent(arg_check_wms_crs(bare, "x", "EPSG:4326"))
})
