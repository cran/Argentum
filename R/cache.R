# Two-tier cache: a session-lifetime environment in front of a persistent
# on-disk store under tools::R_user_dir(). Both honour argentum.cache_ttl.

.arg_mem <- new.env(parent = emptyenv())

#' Where argentum stores cached documents
#'
#' Downloads of the endpoint catalogue and of `GetCapabilities` documents are
#' cached on disk so that repeated calls in a session, or across sessions, do
#' not hammer public servers.
#'
#' @return A single file path. The directory is created on first use.
#' @export
#' @examples
#' argentum_cache_path()
argentum_cache_path <- function() {
  path <- tools::R_user_dir("argentum", which = "cache")
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

#' Clear the argentum cache
#'
#' @param what Which tier to clear: `"all"`, `"memory"` or `"disk"`.
#' @return Invisibly, the number of on-disk files removed.
#' @export
#' @examples
#' argentum_cache_clear("memory")
argentum_cache_clear <- function(what = c("all", "memory", "disk")) {
  what <- match.arg(what)
  removed <- 0L
  if (what %in% c("all", "memory")) {
    rm(list = ls(.arg_mem, all.names = TRUE), envir = .arg_mem)
  }
  if (what %in% c("all", "disk")) {
    files <- list.files(argentum_cache_path(), full.names = TRUE)
    removed <- sum(file.remove(files))
  }
  arg_inform("Cache cleared ({what}).")
  invisible(removed)
}

#' @noRd
arg_cache_key <- function(...) {
  paste0(substr(rlang::hash(list(...)), 1, 16), ".rds")
}

#' Fetch a value from cache, computing it if absent or stale
#' @noRd
arg_cached <- function(key, compute, refresh = FALSE, ttl = arg_opt("argentum.cache_ttl")) {
  file <- file.path(argentum_cache_path(), key)

  if (!refresh) {
    if (!is.null(.arg_mem[[key]])) return(.arg_mem[[key]])
    if (file.exists(file) && difftime(Sys.time(), file.mtime(file), units = "secs") < ttl) {
      value <- try(readRDS(file), silent = TRUE)
      if (!inherits(value, "try-error")) {
        .arg_mem[[key]] <- value
        return(value)
      }
    }
  }

  value <- compute()
  .arg_mem[[key]] <- value
  try(saveRDS(value, file), silent = TRUE)
  value
}
