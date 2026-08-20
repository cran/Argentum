# Centralised HTTP layer -------------------------------------------------
# Every network call in the package goes through arg_request()/arg_perform()
# so that user agent, timeout, retry policy and error classification are
# defined in exactly one place.

#' @noRd
arg_request <- function(url,
                        query = list(),
                        timeout = arg_opt("argentum.timeout"),
                        max_tries = arg_opt("argentum.max_tries")) {
  query <- Filter(Negate(is.null), query)

  # Overwrite rather than append. A base URL that already carries
  # `?service=WFS&request=GetCapabilities` would otherwise end up with the
  # parameter twice, and which one the server honours is anybody's guess.
  if (length(query)) url <- arg_drop_query_params(url, names(query))

  req <- httr2::request(url)
  req <- httr2::req_user_agent(req, arg_user_agent())
  req <- httr2::req_timeout(req, timeout)
  if (length(query)) req <- httr2::req_url_query(req, !!!query)
  req <- httr2::req_retry(
    req,
    max_tries = max_tries,
    backoff = function(i) min(2^i, 30),
    is_transient = function(resp) {
      httr2::resp_status(resp) %in% c(408, 429, 500, 502, 503, 504)
    }
  )
  httr2::req_error(req, body = arg_http_error_body)
}

#' @noRd
arg_user_agent <- function() {
  paste0(
    "Argentum/", utils::packageVersion("Argentum"),
    " (https://github.com/thomasartopoulos/argentum)"
  )
}

# Many Argentine servers answer errors with an OGC ServiceExceptionReport and
# an HTTP 200. Surface that text instead of a bare status code.
#' @noRd
arg_http_error_body <- function(resp) {
  txt <- try(httr2::resp_body_string(resp), silent = TRUE)
  if (inherits(txt, "try-error") || !nzchar(txt)) return(NULL)
  exc <- try({
    x <- arg_read_xml(txt)
    xml2::xml_ns_strip(x)
    xml2::xml_text(xml2::xml_find_first(x, arg_xpath(c("ServiceException", "ExceptionText"))))
  }, silent = TRUE)
  if (!inherits(exc, "try-error") && !is.na(exc) && nzchar(exc)) {
    return(paste("Server said:", trimws(exc)))
  }
  substr(trimws(txt), 1, 200)
}

#' @noRd
arg_perform <- function(req, what = "request") {
  rlang::try_fetch(
    httr2::req_perform(req),
    httr2_failure = function(cnd) {
      cli::cli_abort(
        c(
          "Could not reach the service while performing the {what}.",
          "i" = "The endpoint may be offline, or you may be behind a proxy."
        ),
        parent = cnd,
        class = "argentum_error_offline"
      )
    },
    httr2_http = function(cnd) {
      cli::cli_abort(
        "The service rejected the {what}.",
        parent = cnd,
        class = "argentum_error_http"
      )
    }
  )
}

#' @noRd
# .envir has to travel: without it cli evaluates {braces} in this frame and a
# message referring to a caller's variable dies with "object not found".
arg_inform <- function(..., .envir = parent.frame()) {
  if (isTRUE(arg_opt("argentum.quiet"))) return(invisible())
  cli::cli_inform(..., .envir = .envir)
}

# Detect an OGC ServiceExceptionReport in a 200-OK body.
#' @noRd
arg_check_exception <- function(xml) {
  node <- xml2::xml_find_first(xml, arg_xpath(c("ServiceException", "ExceptionText")))
  if (!is.na(node)) {
    cli::cli_abort(
      c("The service returned an OGC exception.",
        "x" = trimws(xml2::xml_text(node))),
      class = "argentum_error_ogc"
    )
  }
  invisible(xml)
}
