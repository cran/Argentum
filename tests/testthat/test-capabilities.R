test_that("the request base keeps everything except OGC operation parameters", {
  expect_equal(
    arg_strip_ogc_params("https://x/ows?service=WMS&request=Get"),
    "https://x/ows"
  )
  expect_equal(arg_strip_ogc_params("  https://x/ows  "), "https://x/ows")
})

test_that("WFS layers are parsed, including missing optional elements", {
  layers <- arg_parse_wfs_layers(as_capabilities(wfs_capabilities_xml(), "WFS", "2.0.0"))

  expect_equal(nrow(layers), 2L)
  expect_equal(layers$name, c("ign:provincia", "ign:localidad"))
  expect_equal(layers$title[1], "Provincias")
  # A layer without a Title falls back to its Name
  expect_equal(layers$title[2], "ign:localidad")
  expect_equal(layers$bbox[1], "-73.6,-55.1,-53.6,-21.8")
})

test_that("WMS parsing skips container layers that have no Name", {
  layers <- arg_parse_wms_layers(as_capabilities(wms_capabilities_xml(), "WMS", "1.3.0"))

  expect_equal(nrow(layers), 1L)
  expect_equal(layers$name, "capabaseargenmap")
  expect_false("Container without a Name" %in% layers$title)
  expect_equal(layers$bbox, "-73.6,-55.1,-53.6,-21.8")
})

test_that("a service with no layers yields zero rows, not an error", {
  empty <- as_capabilities(xml2::read_xml("<WFS_Capabilities/>"), "WFS", "2.0.0")
  expect_equal(nrow(arg_parse_wfs_layers(empty)), 0L)
  expect_named(arg_parse_wfs_layers(empty), c("name", "title", "abstract", "crs", "bbox"))
})

test_that("OGC exceptions returned with HTTP 200 are detected", {
  xml <- xml2::read_xml(
    '<ServiceExceptionReport><ServiceException>Layer not defined</ServiceException></ServiceExceptionReport>'
  )
  expect_error(arg_check_exception(xml), class = "argentum_error_ogc")
  expect_error(arg_check_exception(xml), "Layer not defined")
})

test_that("capabilities objects print their negotiated version", {
  cap <- as_capabilities(wfs_capabilities_xml(), "WFS", "2.0.0")
  # cli routes to stdout or stderr depending on the handler in force, so look
  # at both rather than tying the test to one of them.
  printed <- paste(
    utils::capture.output(print(cap), type = "output"),
    utils::capture.output(print(cap), type = "message"),
    collapse = " "
  )
  expect_match(printed, "WFS 2.0.0", fixed = TRUE)
})

test_that("version comparison orders protocol versions correctly", {
  expect_true(arg_version_gte("2.0.0", "2.0.0"))
  expect_true(arg_version_gte("1.3.0", "1.1.1"))
  expect_false(arg_version_gte("1.1.0", "2.0.0"))
})

test_that("CRS identifiers are normalised from every spelling in the wild", {
  expect_equal(arg_normalize_crs("EPSG:4326"), "EPSG:4326")
  expect_equal(arg_normalize_crs("urn:ogc:def:crs:EPSG::22185"), "EPSG:22185")
  expect_equal(arg_normalize_crs("http://www.opengis.net/def/crs/EPSG/0/3857"), "EPSG:3857")
  expect_equal(arg_normalize_crs("CRS:84"), "CRS:84")
  expect_equal(arg_normalize_crs(""), "")
})

test_that("a layer's CRS list gathers every declaration, WFS and WMS alike", {
  wfs <- as_capabilities(wfs_capabilities_xml(), "WFS", "2.0.0")
  expect_equal(
    arg_layer_crs_list(wfs, "ign:provincia", "wfs"),
    c("EPSG:22185", "EPSG:4326")
  )

  # WMS inherits CRS down the layer tree, so the parent's counts too
  wms <- as_capabilities(wms_capabilities_xml(), "WMS", "1.3.0")
  expect_setequal(
    arg_layer_crs_list(wms, "capabaseargenmap", "wms"),
    c("EPSG:4326", "EPSG:3857")
  )
})

test_that("CRS selection prefers the caller's, then the package list", {
  available <- c("EPSG:22185", "EPSG:4326", "EPSG:3857")

  expect_equal(arg_choose_crs(available, preferred = "EPSG:3857"), "EPSG:3857")
  # Without a preference, EPSG:4326 wins over the layer's own default
  expect_equal(arg_choose_crs(available), "EPSG:4326")
  # Nothing familiar: take what the layer declares first
  expect_equal(arg_choose_crs(c("EPSG:22174", "EPSG:5348")), "EPSG:22174")
  # Nothing declared: say so, rather than inventing EPSG:4326
  expect_null(arg_choose_crs(character(0)))
})

test_that("image format is negotiated against what GetMap advertises", {
  cap <- as_capabilities(wms_capabilities_xml(), "WMS", "1.3.0")
  expect_equal(arg_wms_formats(cap), c("image/jpeg", "image/png"))

  expect_equal(arg_choose_format(arg_wms_formats(cap)), "image/png")
  expect_equal(arg_choose_format(arg_wms_formats(cap), "image/jpeg"), "image/jpeg")
  # Asking for something unpublished falls back to the preference list
  expect_equal(arg_choose_format(c("image/gif")), "image/gif")
  expect_equal(arg_choose_format(character(0)), "image/png")
})

test_that("layer names with a quote do not break the XPath", {
  expect_equal(arg_xpath_literal("simple"), "'simple'")
  expect_match(arg_xpath_literal("O'Higgins"), "^concat\\(")
})

test_that("a prefixed namespace does not hide the layers", {
  # xml_ns_strip() only removes default namespaces, so this is the case that
  # silently returned zero layers against every GeoServer.
  cap <- as_capabilities(wfs_capabilities_prefixed(), "WFS", "2.0.0")
  layers <- arg_parse_wfs_layers(cap)

  expect_equal(nrow(layers), 1L)
  expect_equal(layers$name, "ide:rutas")
  expect_equal(layers$crs, "EPSG:22185")
  expect_equal(arg_layer_crs_list(cap, "ide:rutas", "wfs"), c("EPSG:22185", "EPSG:4326"))
})

test_that("WMS 1.1.1 parses, SRS and all", {
  cap <- as_capabilities(wms_capabilities_111(), "WMS", "1.1.1")
  layers <- arg_parse_wms_layers(cap)

  expect_equal(layers$name, "vieja:1")
  # One element can carry several codes, separated by spaces
  expect_equal(
    arg_layer_crs_list(cap, "vieja:1", "wms"),
    c("EPSG:4326", "EPSG:22185")
  )
  expect_equal(arg_choose_format(arg_wms_formats(cap)), "image/gif")
})

test_that("capabilities survive the on-disk cache across a session restart", {
  # The regression that R CMD check --run-donttest caught: the cache stored
  # the parsed xml_document, saveRDS() wrote its external pointer as NULL, and
  # the next R session got "external pointer is not valid" on first use.
  local_quiet()
  local_temp_cache()

  record_from_fixture <- list(
    text = as.character(wfs_capabilities_xml()),
    service = "WFS", version = "2.0.0", url = "https://x/ows"
  )
  testthat::local_mocked_bindings(
    arg_fetch_capabilities = function(base, service, versions) record_from_fixture
  )

  first <- argentum_capabilities("https://x/ows", "wfs")
  expect_equal(nrow(arg_parse_wfs_layers(first)), 2L)

  # Simulate a restart: the memory tier is gone, the disk tier remains.
  argentum_cache_clear("memory")
  again <- argentum_capabilities("https://x/ows", "wfs")
  expect_s3_class(again, "argentum_capabilities")
  expect_equal(attr(again, "version"), "2.0.0")
  expect_equal(nrow(arg_parse_wfs_layers(again)), 2L)   # pointer must be live
})

test_that("a legacy cache entry holding a parsed document is recomputed", {
  local_quiet()
  local_temp_cache()

  # Plant what argentum <= 2.0 wrote: the object itself, pointer and all.
  key <- arg_cache_key("cap", "https://x/ows", "wfs", WFS_VERSIONS)
  corpse <- as_capabilities(wfs_capabilities_xml(), "WFS", "2.0.0")
  saveRDS(corpse, file.path(argentum_cache_path(), key))
  argentum_cache_clear("memory")

  fetched <- 0L
  testthat::local_mocked_bindings(
    arg_fetch_capabilities = function(base, service, versions) {
      fetched <<- fetched + 1L
      list(text = as.character(wfs_capabilities_xml()),
           service = "WFS", version = "2.0.0", url = "https://x/ows")
    }
  )

  cap <- argentum_capabilities("https://x/ows", "wfs")
  expect_equal(fetched, 1L)
  expect_equal(nrow(arg_parse_wfs_layers(cap)), 2L)
})

test_that("an OGC exception with a prefixed namespace is still detected", {
  xml <- xml2::read_xml(paste0(
    '<ows:ExceptionReport xmlns:ows="http://www.opengis.net/ows/1.1">',
    "<ows:Exception><ows:ExceptionText>Layer not defined</ows:ExceptionText>",
    "</ows:Exception></ows:ExceptionReport>"
  ))
  expect_error(arg_check_exception(xml), class = "argentum_error_ogc")
  expect_error(arg_check_exception(xml), "Layer not defined")
})
