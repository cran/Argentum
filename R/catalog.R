# Endpoint catalogue -----------------------------------------------------
# v1 read five hard-coded Datawrapper CDN URLs that carry a version number in
# the path, so the package broke silently whenever a chart was republished.
# v2 kept that source and only discovered the version at runtime, which did not
# help once IDERA stopped publishing those charts altogether.
#
# v2.1 reads the same spreadsheet that feeds IDERA's own Buscador de
# Geoservicios, exactly as the ArgentinaGeoServices QGIS plugin does. It is one
# download, it has no version number in the URL, it carries a modification date
# per record, and it lists WCS and CSW alongside WMS and WFS.
#
# The catalogue is still resolved in four layers, first hit wins:
#
#   1. options(argentum.catalog = "path/to.csv")   user override
#   2. the on-disk cache, if fresher than argentum.cache_ttl
#   3. the IDERA spreadsheet
#   4. the snapshot bundled in inst/extdata/organizations.csv
#
# Layer 4 means the package still works offline and on CRAN checks.

#' Public identifier of the published sheet
#'
#' NOT a secret, even though credential scanners flag it for its entropy: it is
#' the identifier Google generates on "Publish to the web" and it only enables
#' anonymous reading of that one sheet. IDERA's own Buscador de Geoservicios
#' ships the same value in its client-side JavaScript. It grants no account
#' access and cannot write.
#' @noRd
ARG_PUBLISHED_SHEET_ID <- "2PACX-1vTC_6dXWs9MCP2cxCJquyvJzW_tXO4g2hcbEfyOyQ5BlbKSX7iKxdKBjuUwlEqJJELIJgLERDp5ZUir" # pragma: allowlist secret

#' The "Geoservicios_IDERA" sheet, published as CSV
#'
#' Same source as <https://www.idera.gob.ar/index.php/servicios/geoservicios>.
#' It arrives in long format: one row per (organization, service type).
#' @noRd
ARG_IDERA_URL <- sprintf(
  "https://docs.google.com/spreadsheets/d/e/%s/pub?gid=0&single=true&output=csv",
  ARG_PUBLISHED_SHEET_ID
)

#' Service types the spreadsheet publishes. The package reads the first two.
#' @noRd
ARG_SERVICE_TYPES <- c("wms", "wfs", "wcs", "csw")

#' @noRd
ARG_URL_COLS <- paste0(ARG_SERVICE_TYPES, "_url")

#' @noRd
CATALOG_COLS <- c(
  "level", "jurisdiction", "dependency", "organization", "updated", ARG_URL_COLS
)

# Jurisdictional levels, in the order IDERA uses. A level that appears in the
# spreadsheet and not here is still kept; it just sorts last. If IDERA adds one,
# nothing here needs editing.
#' @noRd
ARG_LEVELS <- c("Nacional", "Provincial", "Local", "Universidad")

#' List organizations publishing OGC services
#'
#' Returns the catalogue of Argentine public bodies known to expose OGC
#' geoservices, as published in IDERA's own spreadsheet.
#'
#' @param service Keep only organizations offering this service: `"any"`
#'   (default), `"wfs"`, `"wms"`, `"wcs"` or `"csw"`. The package can read WMS
#'   and WFS; `wcs_url` and `csw_url` are carried so you can see what an
#'   organization publishes, and reach it with another tool.
#' @param refresh If `TRUE`, ignore the cache and re-download the catalogue.
#' @return A data frame with columns `name`, `level`, `jurisdiction`,
#'   `dependency`, `organization`, `updated`, `wms_url`, `wfs_url`, `wcs_url`
#'   and `csw_url`. `name` is the human-readable label used by the rest of the
#'   package.
#' @seealso [argentum_search_organizations()], [argentum_layers()]
#' @export
#' @examples
#' \dontrun{
#' orgs <- argentum_organizations()
#' head(orgs$name)
#'
#' # Only those with a usable WMS endpoint
#' argentum_organizations(service = "wms")
#' }
argentum_organizations <- function(service = c("any", "wfs", "wms", "wcs", "csw"),
                                   refresh = FALSE) {
  service <- match.arg(service)

  user_path <- arg_opt("argentum.catalog")
  catalog <- if (!is.null(user_path)) {
    arg_read_catalog_csv(user_path)
  } else {
    arg_cached(
      key = "catalog.rds",
      compute = arg_fetch_catalog,
      refresh = refresh
    )
  }

  if (service == "any") return(catalog)
  catalog[nzchar(catalog[[paste0(service, "_url")]]), , drop = FALSE]
}

#' Search the organization catalogue
#'
#' @param pattern Case-insensitive regular expression matched against `name`.
#' @inheritParams argentum_organizations
#' @return A data frame, possibly with zero rows.
#' @export
#' @examples
#' \dontrun{
#' argentum_search_organizations("buenos aires")
#' argentum_search_organizations("catastro", service = "wfs")
#' }
argentum_search_organizations <- function(pattern,
                                          service = c("any", "wfs", "wms", "wcs", "csw"),
                                          refresh = FALSE) {
  if (!rlang::is_string(pattern)) {
    cli::cli_abort("{.arg pattern} must be a single string.")
  }
  orgs <- argentum_organizations(service = service, refresh = refresh)
  hits <- grepl(pattern, orgs$name, ignore.case = TRUE, perl = TRUE)
  if (!any(hits)) {
    arg_inform("No organization matches {.val {pattern}}.")
  }
  orgs[hits, , drop = FALSE]
}

#' Resolve an organization name or a bare URL into an endpoint
#'
#' Every reader in the package accepts either a catalogue name or a raw
#' service URL. This is the single place where that is decided.
#'
#' @param x A catalogue `name`, or a URL starting with `http`.
#' @param service `"wfs"` or `"wms"`.
#' @return A single URL string.
#' @noRd
arg_resolve_endpoint <- function(x, service = c("wfs", "wms")) {
  service <- match.arg(service)
  if (!rlang::is_string(x)) {
    cli::cli_abort("{.arg organization} must be a single organization name or URL.")
  }
  if (grepl("^https?://", x)) return(x)

  orgs <- argentum_organizations()
  row <- orgs[orgs$name == x, , drop = FALSE]

  if (nrow(row) == 0) {
    near <- orgs$name[utils::adist(x, orgs$name, ignore.case = TRUE) < nchar(x) / 2]
    cli::cli_abort(c(
      "Unknown organization {.val {x}}.",
      "i" = if (length(near)) "Did you mean {.val {utils::head(near, 3)}}?"
            else "Use {.fn argentum_organizations} to list valid names."
    ), class = "argentum_error_unknown_org")
  }

  url <- row[[paste0(service, "_url")]][1]
  if (is.na(url) || !nzchar(url)) {
    other <- arg_other_services(row)
    cli::cli_abort(c(
      "{.val {x}} does not publish a {toupper(service)} endpoint.",
      "i" = if (nzchar(other)) "It publishes {other}, which this package does not read."
    ), class = "argentum_error_no_endpoint")
  }
  url
}

#' Protocols an organization publishes that the package cannot read
#' @noRd
arg_other_services <- function(row) {
  found <- c("wcs", "csw")[vapply(
    c("wcs", "csw"),
    function(s) isTRUE(nzchar(row[[paste0(s, "_url")]][1])),
    logical(1)
  )]
  if (!length(found)) "" else paste(toupper(found), collapse = " and ")
}

# --- catalogue plumbing --------------------------------------------------

#' @noRd
arg_fetch_catalog <- function() {
  remote <- try(arg_fetch_catalog_remote(), silent = TRUE)
  if (!inherits(remote, "try-error") && nrow(remote) > 0) return(remote)

  cli::cli_warn(c(
    "Could not refresh the endpoint catalogue from IDERA.",
    "i" = "Falling back to the snapshot bundled with Argentum {utils::packageVersion('Argentum')}."
  ))
  snapshot <- arg_read_catalog_csv(
    system.file("extdata", "organizations.csv", package = "Argentum")
  )

  # An empty snapshot is not a usable fallback. Saying so beats handing back a
  # zero-row data frame that looks like "Argentina publishes no geoservices".
  if (!nrow(snapshot)) {
    cli::cli_abort(c(
      "The endpoint catalogue is unavailable and the bundled snapshot is empty.",
      "i" = "Run {.file data-raw/refresh_catalog.R} to rebuild the snapshot.",
      "i" = "Or point {.code options(argentum.catalog=)} at a CSV of your own."
    ), class = "argentum_error_no_catalog")
  }
  snapshot
}

#' @noRd
arg_fetch_catalog_remote <- function(url = ARG_IDERA_URL) {
  resp <- arg_perform(arg_request(url), "catalogue download")
  txt <- httr2::resp_body_string(resp)
  if (!nzchar(txt)) stop("the catalogue source returned an empty document")
  arg_finalise_catalog(arg_parse_idera(txt))
}

# --- reading the raw table -----------------------------------------------

#' Sniff the delimiter instead of trusting the file extension
#'
#' Counted on the header line only, as the plugin does: a quoted body field
#' full of commas should not outvote a tab-separated header.
#' @noRd
arg_sniff_delimiter <- function(line) {
  counts <- vapply(c("\t", ";", ","), function(d) {
    if (grepl(d, line, fixed = TRUE)) {
      length(gregexpr(d, line, fixed = TRUE)[[1]])
    } else {
      0L
    }
  }, integer(1))
  if (max(counts) == 0L) "\t" else names(counts)[which.max(counts)]
}

#' Make header names non-empty and unique, the way the plugin does
#' @noRd
arg_unique_headers <- function(columns) {
  columns <- trimws(ifelse(is.na(columns), "", columns))
  columns <- ifelse(nzchar(columns), columns, sprintf("Columna %d", seq_along(columns)))
  seen <- list()
  out <- character(length(columns))
  for (i in seq_along(columns)) {
    name <- columns[i]
    if (is.null(seen[[name]])) {
      seen[[name]] <- 1L
      out[i] <- name
    } else {
      seen[[name]] <- seen[[name]] + 1L
      out[i] <- sprintf("%s (%d)", name, seen[[name]])
    }
  }
  out
}

#' Read the raw CSV/TSV into an all-character data frame
#'
#' Kept separate from the pivot because the spreadsheet arrives in long format
#' and the pivot needs the raw table, not a resolved catalogue.
#' @noRd
arg_read_table <- function(text) {
  text <- sub("^\\ufeff", "", text)
  text <- gsub("\r\n", "\n", text, fixed = TRUE)
  sep <- arg_sniff_delimiter(sub("\n.*$", "", text))

  # Size the table from the widest line rather than from the header. Rows with
  # more fields than the header exist in the wild; read.table() refuses them
  # unless it already knows how many columns to expect.
  widths <- utils::count.fields(textConnection(text), sep = sep, quote = "\"")
  widths <- widths[!is.na(widths)]
  if (!length(widths)) return(arg_empty_table())

  raw <- utils::read.table(
    text = text,
    sep = sep,
    quote = "\"",
    header = FALSE,
    col.names = sprintf("V%d", seq_len(max(widths))),
    colClasses = "character",
    check.names = FALSE,
    na.strings = character(0),
    fill = TRUE,
    comment.char = "",
    blank.lines.skip = TRUE,
    strip.white = TRUE,
    stringsAsFactors = FALSE
  )
  if (nrow(raw) < 2L) return(arg_empty_table())

  df <- raw[-1L, , drop = FALSE]
  names(df) <- arg_unique_headers(as.character(raw[1L, ]))
  df[] <- lapply(df, function(col) trimws(ifelse(is.na(col), "", col)))
  rownames(df) <- NULL

  keep <- Reduce(`|`, lapply(df, nzchar))
  df[keep, , drop = FALSE]
}

#' @noRd
arg_empty_table <- function() {
  data.frame(stringsAsFactors = FALSE)[0, , drop = FALSE]
}

# --- mapping the spreadsheet's hand-edited headers ------------------------

# Accent folding without a dependency, and without iconv(), whose //TRANSLIT
# output differs across platforms.
#' @noRd
ARG_ACCENTED <- paste0(
  "\u00e1\u00e0\u00e4\u00e2\u00e3\u00e9\u00e8\u00eb\u00ea\u00ed\u00ec\u00ef",
  "\u00ee\u00f3\u00f2\u00f6\u00f4\u00f5\u00fa\u00f9\u00fc\u00fb\u00f1\u00e7",
  "\u00c1\u00c0\u00c4\u00c2\u00c3\u00c9\u00c8\u00cb\u00ca\u00cd\u00cc\u00cf",
  "\u00ce\u00d3\u00d2\u00d6\u00d4\u00d5\u00da\u00d9\u00dc\u00db\u00d1\u00c7"
)

#' @noRd
ARG_UNACCENTED <- paste0(
  "aaaaaeeeeiiiiooooouuuunc",
  "AAAAAEEEEIIIIOOOOOUUUUNC"
)

#' Upper case and unaccented, for comparing hand-typed headers and values
#' @noRd
arg_normalize_text <- function(x) {
  toupper(chartr(ARG_ACCENTED, ARG_UNACCENTED, as.character(x)))
}

#' First column whose name contains any of `needles`
#'
#' Matched on keywords rather than exact names on purpose: the spreadsheet is
#' hand-edited and headers such as `INSTITUCION/ ORGANISMO` change shape
#' (spacing, slash, accents) without warning.
#' @noRd
arg_find_column <- function(columns, needles, exclude = character()) {
  normalized <- arg_normalize_text(columns)
  for (i in seq_along(columns)) {
    name <- normalized[i]
    if (any(vapply(needles, grepl, logical(1), x = name, fixed = TRUE))) {
      if (!any(vapply(exclude, grepl, logical(1), x = name, fixed = TRUE))) {
        return(columns[i])
      }
    }
  }
  ""
}

#' Map the spreadsheet's headers onto the roles the package needs
#' @noRd
arg_idera_columns <- function(columns) {
  map <- list(
    level        = arg_find_column(columns, "NIVEL"),
    service      = arg_find_column(columns, "TIPO"),
    url          = arg_find_column(columns, c("URL", "ENLACE", "LINK")),
    organization = arg_find_column(columns, c("ORGANISMO", "INSTITUC")),
    jurisdiction = arg_find_column(columns, "JURISDICCION", exclude = "NIVEL"),
    dependency   = arg_find_column(columns, "DEPENDENCIA"),
    updated      = arg_find_column(columns, c("FECHA", "MODIFIC", "ACTUALIZ"))
  )

  if (!all(nzchar(c(map$url, map$service, map$organization)))) {
    cli::cli_abort(c(
      "The catalogue source is missing the expected columns.",
      "i" = "An organization, a service type and a URL column are required.",
      "x" = "Found: {.field {columns}}"
    ), class = "argentum_error_catalog_shape")
  }

  map$group <- Filter(nzchar, c(map$level, map$jurisdiction, map$dependency, map$organization))
  map
}

#' Protocols named in a "TIPO DE GEOSERVICIO" cell
#'
#' Matched as words, not by equality: the sheet holds "Servicio WMS",
#' "WMS 1.3.0" and "WMS/WFS", and comparing against a fixed list silently
#' discarded those URLs.
#' @noRd
arg_service_types <- function(value) {
  found <- regmatches(
    arg_normalize_text(value),
    gregexpr("\\b(WMS|WFS|WCS|CSW)\\b", arg_normalize_text(value), perl = TRUE)
  )[[1]]
  unique(tolower(found))
}

# --- the pivot ------------------------------------------------------------

#' Pivot the IDERA spreadsheet into one row per organization
#'
#' The sheet is long: one row per (organization, service type). The package
#' needs the opposite, so rows are grouped by organization and each URL is
#' filed under its protocol.
#'
#' @param text The raw CSV.
#' @return A data frame with [CATALOG_COLS], in the order organizations first
#'   appear.
#' @noRd
arg_parse_idera <- function(text) {
  df <- arg_read_table(text)
  if (!nrow(df)) return(arg_empty_catalog())

  map <- arg_idera_columns(names(df))
  column <- function(role) {
    if (nzchar(map[[role]])) df[[map[[role]]]] else rep("", nrow(df))
  }

  level_raw <- trimws(column("level"))
  level <- ifelse(nzchar(level_raw), level_raw, "Otros")

  # Grouped on the normalized value, not the raw text: the sheet is edited by
  # hand and "Nacional", "nacional" and "NACIONAL " are the same level.
  key_parts <- c(
    list(trimws(arg_normalize_text(level))),
    lapply(map$group, function(cl) trimws(arg_normalize_text(df[[cl]])))
  )
  key <- do.call(paste, c(key_parts, list(sep = "\r")))
  groups <- split(seq_len(nrow(df)), factor(key, levels = unique(key)))

  urls <- arg_clean_url(column("url"))
  types <- lapply(column("service"), arg_service_types)

  # Descriptive fields come from the first row that carries them: an
  # organization may have its date filled in on only one of its rows, and it is
  # not always the first.
  pick <- function(values) {
    vapply(groups, function(idx) arg_first_nonempty(values[idx]), character(1))
  }

  services <- vapply(groups, function(idx) {
    found <- c(wms = "", wfs = "", wcs = "", csw = "")
    for (i in idx) {
      if (!nzchar(urls[i])) next
      for (type in types[[i]]) {
        # First URL wins: a row repeated further down is usually the old one,
        # and it used to overwrite the good one.
        if (!nzchar(found[[type]])) found[[type]] <- urls[i]
      }
    }
    found
  }, character(length(ARG_SERVICE_TYPES)))

  out <- data.frame(
    level        = pick(level),
    jurisdiction = pick(trimws(column("jurisdiction"))),
    dependency   = pick(trimws(column("dependency"))),
    organization = pick(trimws(column("organization"))),
    updated      = pick(trimws(column("updated"))),
    stringsAsFactors = FALSE
  )
  for (type in ARG_SERVICE_TYPES) {
    out[[paste0(type, "_url")]] <- unname(services[type, ])
  }

  rownames(out) <- NULL
  out[, CATALOG_COLS, drop = FALSE]
}

#' @noRd
arg_first_nonempty <- function(values) {
  values <- trimws(values[!is.na(values)])
  values <- values[nzchar(values)]
  if (length(values)) values[1] else ""
}

#' @noRd
arg_empty_catalog <- function() {
  empty <- lapply(CATALOG_COLS, function(...) character())
  names(empty) <- CATALOG_COLS
  do.call(data.frame, c(empty, list(stringsAsFactors = FALSE)))
}

# --- finalising -----------------------------------------------------------

#' Build `name`, drop unusable rows, and settle collisions
#' @noRd
arg_finalise_catalog <- function(df) {
  missing <- setdiff(CATALOG_COLS, names(df))
  if (length(missing)) {
    cli::cli_abort("Catalogue is missing column{?s} {.field {missing}}.")
  }

  for (col in ARG_URL_COLS) {
    df[[col]] <- arg_ensure_scheme(arg_clean_url(df[[col]]))
  }

  keep <- Reduce(`|`, lapply(ARG_URL_COLS, function(col) nzchar(df[[col]])))
  df <- df[keep, , drop = FALSE]
  if (!nrow(df)) return(cbind(name = character(), arg_empty_catalog()))

  # A catalogue read from disk may already carry the names a user's scripts
  # refer to; only build them when they are absent.
  if (!"name" %in% names(df) || !all(nzchar(df$name))) {
    df$name <- arg_row_label(df$organization, df$jurisdiction, df$dependency)
  }

  # Two different organizations can end up with the same label; the plugin does
  # not care because it draws them in separate tabs, but here `name` is the
  # lookup key. Disambiguate with the level before giving up on a row.
  clash <- df$name %in% df$name[duplicated(df$name)]
  df$name[clash] <- trimws(paste0(df$name[clash], " (", df$level[clash], ")"))

  df <- df[!duplicated(df$name), , drop = FALSE]
  rownames(df) <- NULL
  df[, c("name", CATALOG_COLS), drop = FALSE]
}

#' The organization's display name
#'
#' Joins the columns that identify it, skipping repeats: the sheet often
#' carries the same province in `JURISDICCION` and in `DEPENDENCIA`, and the
#' label came out as `IGN - IGN` or
#' `Municipalidad de Balcarce - Buenos Aires - Buenos Aires`.
#' @noRd
arg_row_label <- function(organization, jurisdiction, dependency) {
  parts <- cbind(organization, jurisdiction, dependency)
  apply(parts, 1L, function(values) {
    values <- trimws(values[!is.na(values)])
    values <- values[nzchar(values)]
    values <- values[!duplicated(tolower(values))]
    if (length(values)) paste(values, collapse = " - ") else "(unnamed)"
  })
}

#' Position of a level in IDERA's ordering; unknown levels sort last
#' @noRd
arg_level_order <- function(level) {
  match(tolower(trimws(level)), tolower(ARG_LEVELS), nomatch = length(ARG_LEVELS) + 1L)
}

# --- reading a catalogue from disk ---------------------------------------

#' @noRd
arg_read_catalog_csv <- function(path) {
  if (!nzchar(path) || !file.exists(path)) {
    cli::cli_abort(c(
      "Catalogue file not found at {.path {path}}.",
      "i" = "Run {.file data-raw/refresh_catalog.R} to regenerate the snapshot."
    ), class = "argentum_error_no_catalog")
  }
  df <- utils::read.csv(path, colClasses = "character", check.names = FALSE)
  df[] <- lapply(df, function(col) trimws(ifelse(is.na(col), "", col)))

  df <- arg_upgrade_legacy_catalog(df)

  missing <- setdiff(CATALOG_COLS, names(df))
  if (length(missing)) {
    cli::cli_abort("Catalogue is missing column{?s} {.field {missing}}.")
  }
  arg_finalise_catalog(df)
}

#' Accept a catalogue written for argentum <= 2.0.0
#'
#' Those files have `category, organization, wms_url, wfs_url`. Users point
#' `options(argentum.catalog=)` at hand-maintained copies, so reading them has
#' to keep working.
#' @noRd
arg_upgrade_legacy_catalog <- function(df) {
  if ("level" %in% names(df) || !"category" %in% names(df)) return(df)

  # Keep the 1.x/2.0 label so that scripts holding a literal name keep working.
  if (!"name" %in% names(df) && "organization" %in% names(df)) {
    df$name <- trimws(paste(df$category, df$organization, sep = " - "))
  }
  df$level <- df$category
  df$category <- NULL
  for (col in c("jurisdiction", "dependency", "updated", ARG_URL_COLS)) {
    if (!col %in% names(df)) df[[col]] <- rep("", nrow(df))
  }
  df
}
