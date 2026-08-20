# Argentum 2.1.0

The endpoint catalogue now comes from IDERA, and the OGC request layer follows
the same rules as the [ArgentinaGeoServices QGIS
plugin](https://github.com/thomasartopoulos/argentinageoservices-qgis-plugin),
so both tools reach the same services in the same way.

## Breaking changes

* **The catalogue source changed.** 2.0.0 read five Datawrapper charts,
  discovering their version at runtime. IDERA stopped publishing those tables,
  so that source is gone. The catalogue is now the "Geoservicios_IDERA" sheet
  published as CSV - the one behind IDERA's own Buscador de Geoservicios.
* **`argentum_organizations()` returns a different frame.** `category` is
  replaced by `level` and `jurisdiction`, and `dependency`, `updated`,
  `wcs_url` and `csw_url` are new. Full set: `name`, `level`, `jurisdiction`,
  `dependency`, `organization`, `updated`, `wms_url`, `wfs_url`, `wcs_url`,
  `csw_url`.
* **`name` changed value.** It is now built from the organization plus whatever
  identifies it, with repeats removed - `"IGN"` rather than `"Nacional - IGN"`,
  `"Municipalidad de Balcarce - Buenos Aires"` rather than that name repeated
  twice. Scripts holding a literal name need updating; a near-match hint is
  offered on lookup failure. A CSV written for 2.0.0 and passed through
  `options(argentum.catalog=)` keeps its old names.
* `argentum_read_wms(format=)` defaults to `NULL`, meaning "negotiate from what
  the service advertises", instead of `"image/png"`.

## Bug fixes

* **Layers were invisible on every GeoServer.** Capability documents were
  searched with plain XPath after `xml2::xml_ns_strip()`, which removes only
  *default* namespaces. GeoServer answers `<wfs:WFS_Capabilities>` with a
  prefix, so `//FeatureType` matched nothing and the service looked as if it
  published no layers. All capability parsing now matches on local names.
* **The bundled snapshot was empty.** `inst/extdata/organizations.csv` held
  only a header row, so the documented offline fallback returned zero
  organizations. It is regenerated from IDERA, and an empty snapshot now
  raises an error instead of quietly returning nothing.
* **Base URLs lost their query string.** The request base was built with
  `sub("\\?.*$", "")`, which also discarded MapServer's `map=` parameter and
  any access token, breaking those services outright. Only OGC operation
  parameters are removed now. This matters: 59 of the 142 WFS URLs in the
  catalogue arrive carrying `service=`/`version=`/`request=` copied from a
  GetCapabilities, and leaving them in produced "invalid layer" with no
  explanation.
* **Messages lost their variables.** `arg_inform()` did not pass `.envir` to
  cli, so `argentum_search_organizations()` failed with "object \'pattern\' not
  found" instead of reporting that nothing matched.
* URLs from the spreadsheet are cleaned the way the plugin cleans them:
  markdown links, `[WFS]` tags, parenthesised comments, placeholders such as
  "No service available", and invisible characters (NBSP, zero-width space,
  BOM) that made requests resolve against a domain that does not exist. A URL
  broken in half by a stray space is discarded rather than guessed at, because
  gluing the pieces back together invents host names.

## New features

* **WFS reads retry before giving up.** A failed read is retried with paging
  disabled, then against each older protocol version. Several Argentine
  GeoServer installs advertise 2.0.0, fail to serialise GML in it, and answer
  1.0.0 perfectly.
* **Failures quote the server.** When every attempt fails, argentum asks for a
  single feature and reports the OGC exception - "Error encoding object to
  xml-element" - instead of a bare parse failure.
* **CRS is negotiated.** `argentum_read_wfs(crs = NULL)` picks from the CRSs
  the layer declares, preferring EPSG:4326, then 3857, 900913 and 22185. URN
  and OGC-http CRS identifiers are normalised to `EPSG:code`. If the layer
  declares nothing, the server's default is used rather than an invented one.
* **WMS format is negotiated** against the `GetMap` formats the service
  advertises, and a CRS the layer does not publish is refused with the list of
  ones that would work, instead of returning a blank tile.
* The catalogue carries WCS and CSW endpoints. argentum does not read those
  protocols, but it will now tell you an organization publishes them rather
  than reporting it as having no services.
* `argentum_help()` prints a guided, copy-pasteable tour at the console -
  getting started, the catalogue, layers, downloading, WMS and options - in
  Spanish by default and English via `lang = "en"`.
* XML from third-party servers goes through a single hardened entry point:
  documents that declare `<!ENTITY` are rejected, responses are capped at
  64 MiB, and the parser options are pinned (`NONET`, never `NOENT` or
  `HUGE`). Neither XXE nor billion laughs works against xml2's defaults; the
  hardening makes that explicit and tested instead of inherited.

# Argentum 2.0.0

A rewrite of the internals. Every 1.x function still works but now warns;
see `vignette("migrating-to-2-0")`.

## Breaking changes

* `argentum_organizations()` returns lower-case column names (`name`,
  `category`, `organization`, `wms_url`, `wfs_url`). The deprecated
  `argentum_list_organizations()` still returns the old `Name`/`WMS_URL`/
  `WFS_URL` spelling.
* Minimum R version is now 4.1.

## New features

* **WMS is actually implemented.** `argentum_read_wms()` issues a `GetMap`
  request and returns a georeferenced `terra::SpatRaster`;
  `argentum_wms_legend()` fetches the matching legend. Version 1 advertised
  WMS support and collected `WMS_URL`, but no function ever used it.
* `argentum_read_wfs()` replaces `argentum_import_wfs_layer()` and adds
  server-side `bbox` filtering, CQL `filter=`, `crs=` reprojection,
  `max_features=`, and automatic pagination for layers larger than one
  response.
* Protocol versions are negotiated instead of hard-coded. WFS is tried at
  2.0.0, 1.1.0 and 1.0.0; WMS at 1.3.0 and 1.1.1. This is what makes ArcGIS
  Server endpoints work alongside GeoServer.
* If an endpoint cannot produce GeoJSON, the request falls back to GML rather
  than failing.
* WMS 1.3.0 axis order is handled, including the Argentine Gauss-Krüger
  zones (EPSG:22171–22175), which are defined northing-first.
* Results are cached in memory and on disk under `argentum_cache_path()`.
  `argentum_cache_clear()` empties it, `options(argentum.cache_ttl=)` tunes it.
* `argentum_capabilities()` returns a printable object carrying the service,
  the negotiated version and the endpoint.
* `argentum_search_organizations()` filters the catalogue by regular
  expression.
* `argentum_browse()` unifies the two 1.x interactive helpers and handles WMS.
* Behaviour is configurable through options: `argentum.timeout`,
  `argentum.max_tries`, `argentum.page_size`, `argentum.cache_ttl`,
  `argentum.catalog`, `argentum.quiet`.

## Bug fixes

* `argentum_download_layers()` recorded no failures. Its `tryCatch()` handler
  assigned to `result$status` inside the handler's own frame, so the
  modification was made to a copy and discarded. Failed layers were reported
  as `"pending"` and the closing summary counted zero errors.
  `argentum_download()` captures the outcome in the calling frame.
* `clean_url()` was dead code. `argentum_list_organizations()` defined a local
  function of the same name that shadowed it, so the exported and documented
  version never ran.
* The endpoint catalogue no longer depends on five Datawrapper URLs with a
  version number baked into the path. Versions are discovered at runtime, and
  a snapshot bundled with the package is used when the network is unavailable.
* The catalogue is fetched once per session instead of two or three times per
  operation.
* Errors returned as an OGC `ServiceExceptionReport` with HTTP 200 are now
  detected and reported, instead of being handed to `sf` as if they were data.
* Retries use exponential backoff and only fire on transient status codes.
* The download report is pre-allocated rather than grown with `rbind()` in a
  loop.
* Output filenames no longer go through `make.names()`, which turned
  `ign:provincia` into `ign.provincia`.
* Every error is now a classed condition (`argentum_error_http`,
  `argentum_error_offline`, `argentum_error_ogc`, and others), so callers can
  handle them selectively.

## Infrastructure

* The `pkgdown` workflow was malformed YAML — `branches`, `jobs` and `steps`
  were indented at the wrong level, so it had never run. It is fixed, and
  joined by `R-CMD-check` (Ubuntu, macOS, Windows; release, devel, oldrel)
  and test coverage.
* `.RData`, `.Rhistory` and `.Rproj.user/` are no longer committed.
* `news.md` renamed to `NEWS.md` and removed from `.Rbuildignore`, so it
  appears on the pkgdown site and in the CRAN listing.
* Tests no longer require the network: HTTP is replayed from recorded
  fixtures with `httptest2`.

# Argentum 1.0.0

* `argentum_download_layers()`: new function to download layers.
