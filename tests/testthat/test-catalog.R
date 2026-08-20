test_that("the delimiter is sniffed from the header, not assumed", {
  expect_equal(arg_sniff_delimiter("a\tb\tc\td"), "\t")
  expect_equal(arg_sniff_delimiter("a,b,c,d"), ",")
  expect_equal(arg_sniff_delimiter("a;b;c;d"), ";")
  # No delimiter at all: fall back to tab rather than guessing
  expect_equal(arg_sniff_delimiter("single"), "\t")
})

test_that("duplicate and empty header names are made usable", {
  expect_equal(arg_unique_headers(c("URL", "URL", "")), c("URL", "URL (2)", "Columna 3"))
})

test_that("headers are matched by keyword, accents and all", {
  columns <- c(
    "NIVEL JURISDICCIONAL", "JURISDICCION", "DEPENDENCIA",
    "INSTITUCI\u00d3N/ ORGANISMO", "TIPO DE GEOSERVICIO", "URL",
    "FECHA DE MODIFICACION"
  )
  map <- arg_idera_columns(columns)

  expect_equal(map$level, "NIVEL JURISDICCIONAL")
  expect_equal(map$organization, "INSTITUCI\u00d3N/ ORGANISMO")
  expect_equal(map$service, "TIPO DE GEOSERVICIO")
  expect_equal(map$url, "URL")
  expect_equal(map$updated, "FECHA DE MODIFICACION")
  # "NIVEL JURISDICCIONAL" also contains JURISDICCION and must not win it
  expect_equal(map$jurisdiction, "JURISDICCION")
})

test_that("a sheet without the expected columns is rejected", {
  expect_error(arg_parse_idera("A,B\n1,2\n"), class = "argentum_error_catalog_shape")
})

test_that("service types are read as words, not by equality", {
  expect_equal(arg_service_types("WMS"), "wms")
  expect_equal(arg_service_types("Servicio WCS"), "wcs")
  expect_equal(arg_service_types("WMS 1.3.0"), "wms")
  expect_equal(arg_service_types("WMS/WFS"), c("wms", "wfs"))
  expect_equal(arg_service_types(""), character(0))
})

test_that("the long sheet is pivoted to one row per organization", {
  out <- arg_parse_idera(idera_csv())

  expect_equal(nrow(out), 4L)
  expect_equal(out$level, c("Local", "Local", "Nacional", "Universidad"))

  balcarce <- out[1, ]
  expect_equal(balcarce$wms_url, "https://geo.ideba.gob.ar/balcarce/wms")
  expect_equal(balcarce$wfs_url, "https://geo.ideba.gob.ar/balcarce/wfs")
  expect_equal(balcarce$csw_url, "https://geonetwork.ideba.gob.ar/srv/eng/csw")
  expect_equal(balcarce$wcs_url, "")

  # A port in the host must survive the URL cleaner
  expect_equal(
    out$wms_url[2],
    "https://gis.ciudaddecorrientes.gov.ar:8282/geoserver/wms"
  )
  # A quoted field containing a comma must survive the CSV reader
  expect_equal(out$dependency[3], "Ministerio de Economia, Ministerio de Defensa")
})

test_that("a hand-edited sheet still groups correctly", {
  out <- arg_parse_idera(idera_messy_csv())

  # "Nacional", "nacional" and "NACIONAL " are one level, not three
  expect_equal(unique(out$level), "Nacional")
  expect_equal(nrow(out), 2L)

  ign <- out[out$organization == "IGN", ]
  expect_equal(ign$wcs_url, "https://wms.ign.gob.ar/geoserver/wcs")
  # The date is on the second row of the group, not the first
  expect_equal(ign$updated, "2025-03-01")

  inta <- out[out$organization == "INTA", ]
  expect_equal(inta$wms_url, "https://geo.inta.gob.ar/geoserver/ows")
  expect_equal(inta$wfs_url, "https://geo.inta.gob.ar/geoserver/ows")
})

test_that("a repeated row further down does not overwrite the first URL", {
  out <- arg_parse_idera(idera_messy_csv())
  expect_false(any(grepl("vieja", out$wms_url)))
})

test_that("query strings on catalogue URLs are preserved, not stripped", {
  csv <- paste0(
    "NIVEL JURISDICCIONAL,JURISDICCION,DEPENDENCIA,INSTITUCION/ ORGANISMO,",
    "TIPO DE GEOSERVICIO,URL,FECHA DE MODIFICACION\n",
    "Nacional,,IGN,BAHRA,WMS,",
    "https://wms.ign.gob.ar/geoserver/idera/bahra/ows?service=WFS&version=1.3.0&request=GetCapabilities,\n",
    "Nacional,,IGN,BAHRA,CSW,https://catalogo.ign.gob.ar/geonetwork/srv/eng/csw?,\n"
  )
  out <- arg_parse_idera(csv)

  expect_equal(
    out$wms_url,
    "https://wms.ign.gob.ar/geoserver/idera/bahra/ows?service=WFS&version=1.3.0&request=GetCapabilities"
  )
  # The trailing "?" is punctuation, not a query
  expect_equal(out$csw_url, "https://catalogo.ign.gob.ar/geonetwork/srv/eng/csw")
})

test_that("the mislabelled row is still requested as WMS", {
  # The sheet files this URL under WMS while the URL itself says service=WFS.
  # Stripping OGC parameters is what makes the request come out right.
  expect_equal(
    arg_strip_ogc_params(
      "https://wms.ign.gob.ar/geoserver/idera/bahra/ows?service=WFS&version=1.3.0&request=GetCapabilities"
    ),
    "https://wms.ign.gob.ar/geoserver/idera/bahra/ows"
  )
})

test_that("names join what identifies the organization, without repeats", {
  expect_equal(arg_row_label("IGN", "", "IGN"), "IGN")
  expect_equal(
    arg_row_label("Municipalidad de Balcarce", "Buenos Aires", "Buenos Aires"),
    "Municipalidad de Balcarce - Buenos Aires"
  )
  expect_equal(arg_row_label("", "", ""), "(unnamed)")
})

test_that("finalise drops rows with no endpoint and settles name collisions", {
  raw <- arg_parse_idera(idera_csv())
  out <- arg_finalise_catalog(raw)

  expect_true(all(nzchar(out$name)))
  expect_equal(anyDuplicated(out$name), 0L)
  expect_equal(names(out), c("name", CATALOG_COLS))

  none <- raw
  none[, ARG_URL_COLS] <- ""
  expect_equal(nrow(arg_finalise_catalog(none)), 0L)
})

test_that("levels sort in IDERA's order, with unknown ones last", {
  expect_equal(
    order(arg_level_order(c("Universidad", "Local", "Nacional", "Empresa", "Provincial"))),
    c(3L, 5L, 2L, 1L, 4L)
  )
})

test_that("the service filter selects the right rows", {
  with_fake_catalog({
    expect_equal(nrow(argentum_organizations()), 3L)
    expect_equal(nrow(argentum_organizations(service = "wfs")), 2L)
    expect_equal(nrow(argentum_organizations(service = "wms")), 2L)
    expect_equal(nrow(argentum_organizations(service = "csw")), 1L)
  })
})

test_that("search is case insensitive and reports misses", {
  with_fake_catalog({
    expect_equal(nrow(argentum_search_organizations("catastro")), 1L)
    expect_equal(nrow(argentum_search_organizations("CATASTRO")), 1L)
    expect_message(
      expect_equal(nrow(argentum_search_organizations("zzz")), 0L),
      "No organization matches"
    )
  })
  expect_error(argentum_search_organizations(c("a", "b")), "single string")
})

test_that("endpoints resolve from names and pass through URLs", {
  with_fake_catalog({
    expect_equal(arg_resolve_endpoint("IGN", "wfs"), "https://example.org/wfs")
    expect_equal(arg_resolve_endpoint("IGN", "wms"), "https://example.org/wms")
    expect_equal(arg_resolve_endpoint("https://other/ows", "wfs"), "https://other/ows")
  })
})

test_that("resolving an absent or serviceless organization fails informatively", {
  with_fake_catalog({
    expect_error(
      arg_resolve_endpoint("No Existe", "wfs"),
      class = "argentum_error_unknown_org"
    )
    expect_error(
      arg_resolve_endpoint("Solo Catalogo - Buenos Aires", "wfs"),
      class = "argentum_error_no_endpoint"
    )
    # ... and says what the organization does publish
    expect_error(arg_resolve_endpoint("Solo Catalogo - Buenos Aires", "wfs"), "CSW")
  })
})

test_that("a catalogue written for argentum 2.0.0 still reads", {
  path <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(legacy_catalog(), path, row.names = FALSE, na = "")
  withr::local_options(argentum.catalog = path)

  orgs <- argentum_organizations()
  expect_equal(names(orgs), c("name", CATALOG_COLS))
  # The 2.0.0 label is kept so that scripts holding a literal name keep working
  expect_true("Nacional - IGN" %in% orgs$name)
  expect_equal(orgs$level[1], "Nacional")
  expect_equal(orgs$wcs_url, rep("", nrow(orgs)))
})

test_that("a malformed user catalogue is rejected clearly", {
  path <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(wrong = 1), path, row.names = FALSE)
  withr::local_options(argentum.catalog = path)
  expect_error(argentum_organizations(), "missing column")

  withr::local_options(argentum.catalog = "does/not/exist.csv")
  expect_error(argentum_organizations(), class = "argentum_error_no_catalog")
})
