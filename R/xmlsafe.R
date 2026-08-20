# Hardened XML parsing ---------------------------------------------------
# Ported from the ArgentinaGeoServices QGIS plugin (xmlsafe.py). Capability
# documents come from servers this package does not control, listed in a public
# spreadsheet that anyone with edit access can change, so responses are treated
# as hostile.
#
# What was verified, against this same libxml2:
#
# - XXE (external entity reaching a local file): does not apply. xml2 never
#   passes NOENT, and without it libxml2 does not substitute external
#   entities. There is no file disclosure.
# - Billion laughs (nested internal entity expansion): libxml2 already stops
#   runaway expansion unless HUGE is set, which this package never sets.
#
# Neither attack works today, but both defences depend on defaults staying
# defaults. Rejecting `<!ENTITY` declarations outright, capping the response
# size, and pinning the parser options makes the safety explicit and testable
# instead of inherited, and test-xmlsafe.R keeps it that way.

#' Response size cap, matching the plugin's net.MAX_RESPONSE_BYTES
#'
#' The largest GetCapabilities in the Argentine catalogue is around half a
#' megabyte; 64 MiB leaves ample margin while keeping a hostile or broken
#' server from filling memory.
#' @noRd
ARG_XML_MAX_BYTES <- 64 * 1024 * 1024

#' How much of the start of the document is inspected for the DTD internal
#' subset. The prolog of a GetCapabilities fits with room to spare.
#' @noRd
ARG_XML_PROLOG_BYTES <- 16384L

#' TRUE if the prolog declares entities (the door to entity-expansion tricks)
#' @noRd
arg_xml_has_entity <- function(text) {
  prolog <- substr(text, 1L, ARG_XML_PROLOG_BYTES)
  grepl("<!\\s*ENTITY", prolog, ignore.case = TRUE, perl = TRUE)
}

#' `xml2::read_xml()`, hardened for third-party responses
#'
#' Rejects documents that declare XML entities, caps the document size, and
#' pins the parser options: `NOBLANKS` (xml2's default, kept explicit) and
#' `NONET` (never touch the network for DTDs or schemas). `NOENT` and `HUGE`
#' are deliberately absent - the first would enable entity substitution, the
#' second would lift libxml2's own expansion limits.
#'
#' A bare `<!DOCTYPE ...>` is allowed on purpose: WFS 1.0.0 and WMS 1.1.1
#' capabilities legitimately declare the OGC DTD, and without entity
#' declarations it is harmless.
#'
#' This is the only place in the package that parses XML, on purpose: it is
#' hardened once, not five times.
#'
#' @param text The document as a string.
#' @param max_bytes Size cap; exposed as an argument so tests can exercise it
#'   without building a 64 MiB document.
#' @return An `xml_document`.
#' @noRd
arg_read_xml <- function(text, max_bytes = ARG_XML_MAX_BYTES) {
  if (!rlang::is_string(text) || !nzchar(text)) {
    cli::cli_abort("Empty or invalid XML document.", class = "argentum_error_xml")
  }
  if (nchar(text, type = "bytes") > max_bytes) {
    cli::cli_abort(
      "The response exceeds the {format(max_bytes / 1024^2)} MiB cap and was discarded.",
      class = "argentum_error_xml"
    )
  }
  if (arg_xml_has_entity(text)) {
    cli::cli_abort(
      "The response declares XML entities and was discarded for safety.",
      class = "argentum_error_xml"
    )
  }
  rlang::try_fetch(
    xml2::read_xml(text, options = c("NOBLANKS", "NONET")),
    error = function(cnd) {
      cli::cli_abort("The response is not parseable XML.",
                     parent = cnd, class = "argentum_error_xml")
    }
  )
}
