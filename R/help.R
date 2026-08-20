# Console help -----------------------------------------------------------
# A guided tour that lives where the user already is: the console. The
# package's audience includes people whose first language is Spanish and who
# may be new to R, so the guide speaks Spanish by default and English on
# request, and every block ends in code that can be copied and run as-is.

#' Guided help at the console
#'
#' Prints a short, copy-pasteable guide for common tasks: getting started,
#' understanding the catalogue, listing layers, downloading data, rendering
#' WMS maps and tuning options. It complements `?argentum` (the grouped
#' reference) and the vignettes (the long-form articles).
#'
#' @param topic What to explain. One of `"inicio"` (default), `"catalogo"`,
#'   `"capas"`, `"descargar"`, `"wms"` or `"opciones"`. English synonyms
#'   (`"start"`, `"catalog"`, `"layers"`, `"download"`, `"options"`) are
#'   accepted.
#' @param lang `"es"` (default) or `"en"`.
#' @return `topic`, invisibly. Called for its printed output.
#' @export
#' @examples
#' argentum_help()
#' argentum_help("descargar")
#' argentum_help("download", lang = "en")
argentum_help <- function(topic = "inicio", lang = c("es", "en")) {
  lang <- match.arg(lang)
  if (!rlang::is_string(topic)) {
    cli::cli_abort("{.arg topic} must be a single string.", class = "argentum_error_help")
  }

  synonyms <- c(
    start = "inicio", help = "inicio", catalog = "catalogo",
    layers = "capas", download = "descargar", options = "opciones"
  )
  key <- tolower(trimws(topic))
  if (key %in% names(synonyms)) key <- synonyms[[key]]

  valid <- c("inicio", "catalogo", "capas", "descargar", "wms", "opciones")
  if (!key %in% valid) {
    cli::cli_abort(
      c("Unknown help topic {.val {topic}}.",
        "i" = "Valid topics: {.val {valid}}."),
      class = "argentum_error_help"
    )
  }

  if (lang == "es") arg_help_es(key) else arg_help_en(key)
  invisible(key)
}

#' @noRd
arg_help_code <- function(...) {
  cli::cli_code(c(...))
}

#' @noRd
arg_help_es <- function(key) {
  switch(key,
    inicio = {
      cli::cli_h1("argentum: datos geoespaciales p\u00fablicos argentinos")
      cli::cli_text("El flujo t\u00edpico son tres pasos: encontrar el organismo, ver sus capas, traer una.")
      arg_help_code(
        'library(Argentum)',
        '',
        'orgs <- argentum_organizations()             # 1. \u00bfqui\u00e9n publica?',
        'argentum_search_organizations("catastro")    #    ...o buscalo',
        '',
        'ign <- "https://wms.ign.gob.ar/geoserver/ows"',
        'argentum_layers(ign)                         # 2. \u00bfqu\u00e9 capas tiene?',
        '',
        'prov <- argentum_read_wfs(ign, "ign:provincia")  # 3. traela como sf',
        'plot(sf::st_geometry(prov))'
      )
      cli::cli_text("Si prefer\u00eds elegir con men\u00fas, todo eso junto es {.run argentum_browse()}.")
      cli::cli_h2("M\u00e1s ayuda")
      cli::cli_bullets(c(
        "*" = '{.code argentum_help("catalogo")} - qu\u00e9 es el cat\u00e1logo y de d\u00f3nde sale',
        "*" = '{.code argentum_help("capas")} - listar capas y leer sus columnas',
        "*" = '{.code argentum_help("descargar")} - bajar capas a disco',
        "*" = '{.code argentum_help("wms")} - mapas ya dibujados por el servidor',
        "*" = '{.code argentum_help("opciones")} - timeouts, cach\u00e9, reintentos',
        "*" = "{.code ?argentum} - la referencia completa, agrupada por tarea"
      ))
    },
    catalogo = {
      cli::cli_h1("El cat\u00e1logo de organismos")
      cli::cli_text(paste(
        "Sale de la planilla \"Geoservicios_IDERA\", la misma fuente que usa el",
        "Buscador de Geoservicios de IDERA y el plugin ArgentinaGeoServices de QGIS.",
        "Una fila por organismo, con sus endpoints WMS/WFS (y WCS/CSW informativos)."
      ))
      arg_help_code(
        'orgs <- argentum_organizations()          # cacheado un d\u00eda',
        'argentum_organizations(service = "wfs")   # solo los que tienen WFS',
        'argentum_organizations(refresh = TRUE)    # ignorar el cach\u00e9',
        'argentum_search_organizations("mendoza")  # regex, sin may\u00fascula/min\u00fascula'
      )
      cli::cli_text(paste(
        "Columnas: {.field name} (la clave que aceptan las dem\u00e1s funciones),",
        "{.field level} (Nacional/Provincial/Local/Universidad), {.field jurisdiction},",
        "{.field dependency}, {.field organization}, {.field updated} y las cuatro URLs."
      ))
      cli::cli_text('Con tu propio CSV: {.code options(argentum.catalog = "ruta/mi_catalogo.csv")}.')
    },
    capas = {
      cli::cli_h1("Listar las capas de un servicio")
      arg_help_code(
        'capas <- argentum_layers("Instituto Geogr\u00e1fico Nacional")  # nombre del cat\u00e1logo...',
        'capas <- argentum_layers("https://wms.ign.gob.ar/geoserver/ows")  # ...o URL directa',
        'capas <- argentum_layers(url, service = "wms")   # para WMS (default es WFS)',
        'head(capas[, c("name", "title", "crs")])'
      )
      cli::cli_text(paste(
        "{.field name} es el identificador que piden {.fn argentum_read_wfs} y",
        "{.fn argentum_read_wms}; {.field title} es el nombre humano;",
        "{.field bbox} te da un recorte inicial razonable ({.code xmin,ymin,xmax,ymax} en WGS84)."
      ))
    },
    descargar = {
      cli::cli_h1("Bajar capas a disco")
      cli::cli_text("Para una capa puntual, leela y escribila vos:")
      arg_help_code(
        'prov <- argentum_read_wfs(url, "ign:provincia")',
        'sf::st_write(prov, "provincias.gpkg")'
      )
      cli::cli_text("Para varias capas (o todas), {.fn argentum_download} hace el recorrido y no aborta si una falla:")
      arg_help_code(
        'reporte <- argentum_download(url, dir = "datos/ign", format = "gpkg")',
        'subset(reporte, status == "error")   # qu\u00e9 fall\u00f3 y por qu\u00e9'
      )
      cli::cli_bullets(c(
        "*" = "Formatos: {.val gpkg} (recomendado), {.val geojson}, {.val shp}.",
        "*" = "Filtr\u00e1 antes de bajar: {.code argentum_read_wfs(url, capa, bbox = c(...), filter = \"provincia = 'Mendoza'\")} \u2014 el filtro corre en el servidor.",
        "*" = 'Con men\u00fas: {.code argentum_browse(action = "download")}.'
      ))
    },
    wms = {
      cli::cli_h1("WMS: mapas dibujados por el servidor")
      cli::cli_text(paste(
        "WMS no devuelve datos sino una imagen georreferenciada, ideal como mapa base.",
        "El {.arg bbox} es obligatorio: WMS no tiene un pedido de \"todo\"."
      ))
      arg_help_code(
        'base <- argentum_read_wms(',
        '  "https://wms.ign.gob.ar/geoserver/ows",',
        '  layer = "ign:provincia",',
        '  bbox  = c(-59, -35, -57, -34),   # xmin, ymin, xmax, ymax',
        '  width = 1000',
        ')',
        'terra::plotRGB(base)',
        'argentum_wms_legend(url, "capa")   # la leyenda, si la necesit\u00e1s'
      )
      cli::cli_text(paste(
        "El formato de imagen y el CRS se negocian solos contra lo que el servidor",
        "publica; si el CRS que ped\u00eds no est\u00e1, el error te lista los que s\u00ed."
      ))
    },
    opciones = {
      cli::cli_h1("Opciones del paquete")
      arg_help_code(
        'options(',
        '  argentum.timeout   = 30,      # segundos por request',
        '  argentum.max_tries = 3,       # reintentos con backoff',
        '  argentum.page_size = 5000,    # features por p\u00e1gina WFS',
        '  argentum.cache_ttl = 86400,   # vida del cach\u00e9 (segundos)',
        '  argentum.catalog   = NULL,    # tu propio CSV de endpoints',
        '  argentum.quiet     = FALSE    # silenciar mensajes de progreso',
        ')'
      )
      cli::cli_text(paste(
        "El cat\u00e1logo y los GetCapabilities se cachean en disco en",
        "{.code argentum_cache_path()}; {.code argentum_cache_clear()} lo vac\u00eda.",
        "Detalle completo en {.code ?argentum_options}."
      ))
    }
  )
}

#' @noRd
arg_help_en <- function(key) {
  switch(key,
    inicio = {
      cli::cli_h1("argentum: Argentine public geospatial data")
      cli::cli_text("The typical flow is three steps: find the organization, list its layers, read one.")
      arg_help_code(
        'library(Argentum)',
        '',
        'orgs <- argentum_organizations()             # 1. who publishes?',
        'argentum_search_organizations("cadastre")    #    ...or search',
        '',
        'ign <- "https://wms.ign.gob.ar/geoserver/ows"',
        'argentum_layers(ign)                         # 2. which layers?',
        '',
        'prov <- argentum_read_wfs(ign, "ign:provincia")  # 3. read it as sf',
        'plot(sf::st_geometry(prov))'
      )
      cli::cli_text("Prefer menus? All of the above in one call: {.run argentum_browse()}.")
      cli::cli_h2("More help")
      cli::cli_bullets(c(
        "*" = '{.code argentum_help("catalog", lang = "en")} - the catalogue and where it comes from',
        "*" = '{.code argentum_help("layers", lang = "en")} - listing layers',
        "*" = '{.code argentum_help("download", lang = "en")} - writing layers to disk',
        "*" = '{.code argentum_help("wms", lang = "en")} - server-rendered maps',
        "*" = '{.code argentum_help("options", lang = "en")} - timeouts, cache, retries',
        "*" = "{.code ?argentum} - the full reference, grouped by task"
      ))
    },
    catalogo = {
      cli::cli_h1("The organization catalogue")
      cli::cli_text(paste(
        "It comes from the \"Geoservicios_IDERA\" sheet, the same source behind",
        "IDERA's own service finder and the ArgentinaGeoServices QGIS plugin.",
        "One row per organization, with its WMS/WFS endpoints (WCS/CSW informative)."
      ))
      arg_help_code(
        'orgs <- argentum_organizations()          # cached for a day',
        'argentum_organizations(service = "wfs")   # only those with WFS',
        'argentum_organizations(refresh = TRUE)    # bypass the cache',
        'argentum_search_organizations("mendoza")  # case-insensitive regex'
      )
      cli::cli_text('Bring your own CSV with {.code options(argentum.catalog = "path/to.csv")}.')
    },
    capas = {
      cli::cli_h1("Listing the layers of a service")
      arg_help_code(
        'layers <- argentum_layers("Instituto Geogr\u00e1fico Nacional")  # catalogue name...',
        'layers <- argentum_layers("https://wms.ign.gob.ar/geoserver/ows")  # ...or a URL',
        'layers <- argentum_layers(url, service = "wms")   # for WMS (default is WFS)',
        'head(layers[, c("name", "title", "crs")])'
      )
      cli::cli_text(paste(
        "{.field name} is what {.fn argentum_read_wfs} and {.fn argentum_read_wms}",
        "expect; {.field title} is the human name; {.field bbox} gives you a",
        "reasonable starting window ({.code xmin,ymin,xmax,ymax}, WGS84)."
      ))
    },
    descargar = {
      cli::cli_h1("Writing layers to disk")
      cli::cli_text("For one layer, read it and write it yourself:")
      arg_help_code(
        'prov <- argentum_read_wfs(url, "ign:provincia")',
        'sf::st_write(prov, "provinces.gpkg")'
      )
      cli::cli_text("For several (or all), {.fn argentum_download} walks the service and does not abort on one failure:")
      arg_help_code(
        'report <- argentum_download(url, dir = "data/ign", format = "gpkg")',
        'subset(report, status == "error")   # what failed, and why'
      )
      cli::cli_bullets(c(
        "*" = "Formats: {.val gpkg} (recommended), {.val geojson}, {.val shp}.",
        "*" = "Filter before downloading: {.code argentum_read_wfs(url, layer, bbox = c(...), filter = \"...\")} runs server-side.",
        "*" = 'With menus: {.code argentum_browse(action = "download")}.'
      ))
    },
    wms = {
      cli::cli_h1("WMS: maps rendered by the server")
      cli::cli_text(paste(
        "WMS returns a georeferenced picture, ideal as a basemap. {.arg bbox} is",
        "required: WMS has no \"everything\" request."
      ))
      arg_help_code(
        'basemap <- argentum_read_wms(',
        '  "https://wms.ign.gob.ar/geoserver/ows",',
        '  layer = "ign:provincia",',
        '  bbox  = c(-59, -35, -57, -34),   # xmin, ymin, xmax, ymax',
        '  width = 1000',
        ')',
        'terra::plotRGB(basemap)',
        'argentum_wms_legend(url, "layer")   # the legend, if you need it'
      )
      cli::cli_text(paste(
        "Image format and CRS are negotiated against what the server advertises;",
        "if the CRS you ask for is not published, the error lists the ones that are."
      ))
    },
    opciones = {
      cli::cli_h1("Package options")
      arg_help_code(
        'options(',
        '  argentum.timeout   = 30,      # seconds per request',
        '  argentum.max_tries = 3,       # retries with backoff',
        '  argentum.page_size = 5000,    # features per WFS page',
        '  argentum.cache_ttl = 86400,   # cache lifetime (seconds)',
        '  argentum.catalog   = NULL,    # your own endpoint CSV',
        '  argentum.quiet     = FALSE    # silence progress messages',
        ')'
      )
      cli::cli_text(paste(
        "The catalogue and every GetCapabilities are cached on disk under",
        "{.code argentum_cache_path()}; {.code argentum_cache_clear()} empties it.",
        "Full detail in {.code ?argentum_options}."
      ))
    }
  )
}
