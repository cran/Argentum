# Ported from the QGIS plugin's tests/test_pure.py. Every case here is a row
# that actually appears in IDERA's spreadsheet.

test_that("editorial annotations are stripped", {
  expect_equal(arg_clean_url("[WMS](http://x.gob.ar/wms)"), "http://x.gob.ar/wms")
  expect_equal(arg_clean_url("[WFS] http://b.ar/wfs"), "http://b.ar/wfs")
  expect_equal(arg_clean_url("[CSW] http://a.gob.ar/csw"), "http://a.gob.ar/csw")
  expect_equal(arg_clean_url(" (http://x.gob.ar/wms?) "), "http://x.gob.ar/wms")
  expect_equal(
    arg_clean_url("[WFS] https://example.org/ows (solo capas publicas)"),
    "https://example.org/ows"
  )
})

test_that("placeholders for a missing service become empty", {
  expect_equal(arg_clean_url("No service available"), "")
  expect_equal(arg_clean_url("nan"), "")
  expect_equal(arg_clean_url("s/d"), "")
  expect_equal(arg_clean_url("-"), "")
  expect_equal(arg_clean_url(NA), "")
  expect_equal(arg_clean_url("consultar por mail"), "")
  expect_equal(arg_clean_url("no publica"), "")
})

test_that("invisible whitespace is removed", {
  # NBSP either side, then BOM and a zero-width space
  expect_equal(arg_clean_url("\u00a0http://a.gob.ar/wms\u00a0"), "http://a.gob.ar/wms")
  expect_equal(arg_clean_url("\ufeffhttp://a.gob.ar/wms\u200b"), "http://a.gob.ar/wms")
})

test_that("a URL broken by a stray space is discarded, never guessed", {
  # The break fell inside the host name
  expect_equal(
    arg_clean_url("http://nodoide.cat astro.corrientes.gob.ar/geoserver/wfs"),
    ""
  )
  # ... or inside the path
  expect_equal(arg_clean_url("http://a.gob.ar/geoser ver/wms"), "")
  expect_equal(arg_clean_url("http://a.gob.ar/geo\nserver/wms"), "")
  # A host with no path followed by a date is not a broken URL, but there is no
  # way to tell, so it is dropped too.
  expect_equal(arg_clean_url("https://www.ign.gob.ar 27.02.2025"), "")
})

test_that("a URL followed by a comment or another URL keeps the first URL", {
  expect_equal(arg_clean_url("http://a.gob.ar/wms requiere usuario"), "http://a.gob.ar/wms")
  expect_equal(arg_clean_url("http://a.gob.ar/wms 2024.01.01"), "http://a.gob.ar/wms")
  expect_equal(
    arg_clean_url("http://a.gob.ar/wms http://b.gob.ar/wfs"),
    "http://a.gob.ar/wms"
  )
  expect_equal(
    arg_clean_url("https://a.gob.ar/wms, https://b.gob.ar/wfs"),
    "https://a.gob.ar/wms"
  )
  expect_equal(arg_clean_url("https://a.gob.ar/wms;"), "https://a.gob.ar/wms")
})

test_that("clean_url is vectorised and returns character(0) for nothing", {
  expect_equal(
    arg_clean_url(c("[WMS] http://a/wms", "nan", NA)),
    c("http://a/wms", "", "")
  )
  expect_equal(arg_clean_url(character(0)), character(0))
})

test_that("OGC operation parameters are dropped from a base URL", {
  expect_equal(
    arg_strip_ogc_params(
      "https://wms.ign.gob.ar/geoserver/idera/bahra/ows?service=WFS&version=1.3.0&request=GetCapabilities"
    ),
    "https://wms.ign.gob.ar/geoserver/idera/bahra/ows"
  )
  expect_equal(
    arg_strip_ogc_params("http://a.gob.ar/wfs?acceptversions=2.0.0"),
    "http://a.gob.ar/wfs"
  )
  expect_equal(
    arg_strip_ogc_params("http://a.gob.ar/geoserver/wfs"),
    "http://a.gob.ar/geoserver/wfs"
  )
  expect_equal(arg_strip_ogc_params(""), "")
})

test_that("everything that is not an OGC operation parameter is preserved", {
  # MapServer cannot answer at all without its mapfile
  expect_equal(
    arg_strip_ogc_params(
      "http://a.gob.ar/cgi-bin/mapserv?map=/data/ide.map&service=WMS&request=GetCapabilities"
    ),
    "http://a.gob.ar/cgi-bin/mapserv?map=%2Fdata%2Fide.map"
  )
  expect_equal(
    arg_strip_ogc_params("http://a.gob.ar/wfs?token=abc123&request=GetCapabilities"),
    "http://a.gob.ar/wfs?token=abc123"
  )
})

test_that("a bare host gets a scheme", {
  expect_equal(arg_ensure_scheme("x.gob.ar/wms"), "http://x.gob.ar/wms")
  expect_equal(arg_ensure_scheme("https://x.gob.ar/wms"), "https://x.gob.ar/wms")
  expect_equal(arg_ensure_scheme(""), "")
})
