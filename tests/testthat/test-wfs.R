test_that("bbox accepts numeric input and appends the CRS", {
  expect_equal(arg_bbox_string(c(-59, -35, -57, -34)), "-59,-35,-57,-34")
  expect_equal(
    arg_bbox_string(c(-59, -35, -57, -34), "EPSG:4326"),
    "-59,-35,-57,-34,EPSG:4326"
  )
  expect_null(arg_bbox_string(NULL))
})

test_that("bbox rejects the wrong number of values", {
  expect_error(arg_bbox_string(c(1, 2, 3)), "four values")
})

test_that("crs accepts EPSG codes and strings", {
  expect_equal(arg_crs_string(4326), "EPSG:4326")
  expect_equal(arg_crs_string("EPSG:22185"), "EPSG:22185")
  expect_null(arg_crs_string(NULL))
  expect_error(arg_crs_string(list()), "EPSG code")
})

test_that("layer must be a single string", {
  expect_error(argentum_read_wfs("https://x/ows", c("a", "b")), "single string")
})

test_that("the retry plan degrades from the negotiated version downwards", {
  plan <- arg_wfs_attempt_plan("2.0.0")
  versions <- vapply(plan, function(a) a$version, character(1))

  # Most capable first: negotiated version, with paging
  expect_true(plan[[1]]$paging)
  expect_equal(plan[[1]]$version, "2.0.0")
  # Then the same version without paging, then each older one
  expect_false(plan[[2]]$paging)
  expect_equal(versions, c("2.0.0", "2.0.0", "1.1.0", "1.0.0"))
  expect_false(any(vapply(plan[-1], function(a) a$paging, logical(1))))
})

test_that("the retry plan never climbs back up a version", {
  expect_equal(
    vapply(arg_wfs_attempt_plan("1.1.0"), function(a) a$version, character(1)),
    c("1.1.0", "1.1.0", "1.0.0")
  )
  expect_length(arg_wfs_attempt_plan("1.0.0"), 2L)
  # A version we do not know about still gets the full ladder below it
  expect_equal(
    vapply(arg_wfs_attempt_plan("3.0.0"), function(a) a$version, character(1)),
    c("3.0.0", "3.0.0", "2.0.0", "1.1.0", "1.0.0")
  )
})

test_that("an OGC exception report is turned into the server's own words", {
  expect_equal(
    arg_exception_text(paste0(
      "<ows:ExceptionReport xmlns:ows='http://www.opengis.net/ows/1.1'>",
      "<ows:Exception><ows:ExceptionText>Error encoding object to xml-element",
      "</ows:ExceptionText></ows:Exception></ows:ExceptionReport>"
    )),
    "Error encoding object to xml-element"
  )
  # A normal response is not an exception, and neither is junk
  expect_equal(arg_exception_text("<wfs:FeatureCollection xmlns:wfs='x'/>"), "")
  expect_equal(arg_exception_text("not xml at all"), "")
})

test_that("request URLs overwrite parameters the base already carries", {
  expect_equal(
    arg_query_url(
      "https://geoportal.corrientes.gob.ar/geoserver/wfs?service=WFS&request=GetCapabilities",
      list(service = "WFS", request = "GetFeature", typeNames = "ide:Arroz")
    ),
    paste0(
      "https://geoportal.corrientes.gob.ar/geoserver/wfs",
      "?service=WFS&request=GetFeature&typeNames=ide%3AArroz"
    )
  )
})
