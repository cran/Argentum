# URL hygiene ------------------------------------------------------------
# Ported from the ArgentinaGeoServices QGIS plugin (net.py). Every rule here
# exists because a real row of the IDERA spreadsheet broke something; see
# data-raw/CATALOGO-IDERA.md for the provenance of each one.

# Invisible characters that survive a copy/paste into a spreadsheet: NBSP,
# zero-width space/non-joiner/joiner, word joiner, BOM. Left in place they make
# the URL parser cut the host and the request fails against a domain that does
# not exist.
ARG_INVISIBLE <- "[\u00a0\u200b\u200c\u200d\u2060\ufeff]"

# Cell values that mean "no service", written by hand in a dozen ways.
ARG_EMPTY_VALUES <- c(
  "", "nan", "none", "-", "no service available", "s/d", "n/a", "sin servicio"
)

# OGC operation parameters. The package supplies these on every request; if
# they are also present in the base URL they end up duplicated or, worse, the
# stale value wins and a GetFeature is answered as a GetCapabilities.
#
# Only operation parameters are removed. Everything else is kept on purpose:
# MapServer's `map=/path/to/mapfile` and per-server access tokens are load
# bearing, and dropping them breaks the service outright.
ARG_OGC_PARAMS <- c(
  "service", "request", "version", "acceptversions", "typename", "typenames",
  "layers", "layer", "srsname", "srs", "crs", "bbox", "outputformat", "format",
  "maxfeatures", "count", "resulttype", "startindex", "styles", "style",
  "width", "height", "transparent", "exceptions", "featureid"
)

#' Split a URL into its base and an ordered list of query parameters
#' @noRd
arg_split_url <- function(url) {
  url <- sub("#.*$", "", trimws(url))
  if (!grepl("?", url, fixed = TRUE)) {
    return(list(base = url, query = list()))
  }
  base <- sub("\\?.*$", "", url)
  raw <- sub("^[^?]*\\?", "", url)

  pairs <- strsplit(raw, "&", fixed = TRUE)[[1]]
  pairs <- pairs[nzchar(pairs)]

  query <- lapply(pairs, function(pair) {
    key <- sub("=.*$", "", pair)
    value <- if (grepl("=", pair, fixed = TRUE)) sub("^[^=]*=", "", pair) else ""
    list(key = arg_url_decode(key), value = arg_url_decode(value))
  })
  list(base = base, query = query)
}

#' @noRd
arg_unsplit_url <- function(base, query) {
  if (!length(query)) return(base)
  encoded <- vapply(query, function(p) {
    paste0(arg_url_encode(p$key), "=", arg_url_encode(p$value))
  }, character(1))
  paste0(base, "?", paste(encoded, collapse = "&"))
}

#' @noRd
arg_url_decode <- function(x) {
  out <- try(utils::URLdecode(x), silent = TRUE)
  if (inherits(out, "try-error")) x else out
}

#' @noRd
arg_url_encode <- function(x) {
  utils::URLencode(x, reserved = TRUE)
}

#' Drop named query parameters from a URL, preserving the rest
#'
#' Case-insensitive on the parameter name, because servers are inconsistent
#' about `SERVICE` versus `service`.
#' @noRd
arg_drop_query_params <- function(url, names) {
  url <- trimws(url %||% "")
  if (!nzchar(url)) return("")
  names <- tolower(names)
  parts <- arg_split_url(url)
  kept <- Filter(function(p) !(tolower(p$key) %in% names), parts$query)
  arg_unsplit_url(parts$base, kept)
}

#' Prepare a catalogue URL for use as a request base
#'
#' Many catalogue URLs are copied straight out of a browser that was pointed at
#' a `GetCapabilities`, so they arrive carrying
#' `?service=WFS&version=2.0.0&request=GetCapabilities`. In the QGIS plugin, 59
#' of the 142 WFS URLs in the spreadsheet do this. Passing such a URL to a
#' client that then appends its own parameters produces a request the server
#' answers with the wrong document, or refuses outright.
#'
#' @param url A service URL.
#' @return The URL with OGC operation parameters removed and everything else
#'   preserved.
#' @noRd
arg_strip_ogc_params <- function(url) {
  arg_drop_query_params(url, ARG_OGC_PARAMS)
}

#' Add a scheme to a bare host
#' @noRd
arg_ensure_scheme <- function(url) {
  url <- trimws(url)
  ifelse(!nzchar(url) | grepl("://", url, fixed = TRUE), url, paste0("http://", url))
}

# --- catalogue cell cleaning ---------------------------------------------

#' @noRd
arg_is_url <- function(x) grepl("^https?://", x, ignore.case = TRUE)

# TRUE once there is a slash past the host: the URL got as far as a path.
#' @noRd
arg_has_path <- function(url) {
  grepl("/", sub("^[^:]*://", "", url), fixed = TRUE)
}

# Punctuation that comes along for the ride when a cell lists several URLs.
#' @noRd
arg_trim_url <- function(url) sub("[?&,;.]+$", "", url)

#' Pick the URL out of a cell that got split into pieces
#'
#' Deliberately does NOT try to rebuild a URL by gluing pieces back together.
#' `http://nodoide.cat astro.corrientes.gob.ar/wfs` is a domain cut in half,
#' but `https://www.ign.gob.ar 27.02.2025` is a URL followed by a date.
#' Guessing invents domains that fail anyway, only with a more confusing
#' message. When the break is ambiguous this returns `""`, the organization is
#' listed without that service, and the user can see why.
#' @noRd
arg_pick_url <- function(pieces) {
  for (i in seq_along(pieces)) {
    if (!arg_is_url(pieces[i])) next
    following <- if (i < length(pieces)) pieces[i + 1L] else ""
    if (nzchar(following) && !arg_is_url(following)) {
      if (!arg_has_path(pieces[i])) return("")            # break fell inside the host
      if (grepl("/", following, fixed = TRUE)) return("") # break fell inside the path
    }
    return(arg_trim_url(pieces[i]))
  }
  ""
}

#' Normalise a service URL as it comes out of the catalogue
#'
#' Handles `[WMS](http://...)`, `[WFS] http://...`, URLs wrapped in
#' parentheses, free-text placeholders such as "No service available", and the
#' invisible whitespace that copy/paste leaves behind.
#'
#' @param url A character vector of raw cell values.
#' @return A character vector of URLs; `""` where the cell held no usable one.
#' @noRd
arg_clean_url <- function(url) {
  if (!length(url)) return(character(0))
  vapply(url, arg_clean_url_one, character(1), USE.NAMES = FALSE)
}

#' @noRd
arg_clean_url_one <- function(value) {
  if (is.null(value) || length(value) != 1L || is.na(value)) return("")

  text <- gsub(ARG_INVISIBLE, " ", as.character(value), perl = TRUE)
  text <- trimws(gsub("[[:space:]]+", " ", text, perl = TRUE))
  if (tolower(text) %in% ARG_EMPTY_VALUES) return("")

  md <- regmatches(text, regexec("^\\[[^\\]]*\\]\\(([^)]+)\\)$", text, perl = TRUE))[[1]]
  if (length(md) == 2L) text <- md[2]

  text <- gsub("\\[(WMS|WFS|WCS|WMTS|CSW)\\]", "", text, ignore.case = TRUE, perl = TRUE)
  text <- trimws(gsub("[()]", "", text))

  pieces <- strsplit(text, " ", fixed = TRUE)[[1]]
  pieces <- pieces[nzchar(pieces)]
  if (!length(pieces)) return("")

  if (any(arg_is_url(pieces))) return(arg_pick_url(pieces))
  if (length(pieces) > 1L) return("")                       # free text, not a URL
  if (!grepl(".", pieces[1], fixed = TRUE)) return("")
  arg_trim_url(pieces[1])
}
