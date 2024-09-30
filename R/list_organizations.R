#' List Organizations with WMS and WFS Services
#'
#' @return A data frame containing organization information including WMS and WFS URLs
#' @export
argentum_list_organizations <- function() {
  urls <- c(
    "https://datawrapper.dwcdn.net/nH8e7/45/dataset.csv",
    "https://datawrapper.dwcdn.net/JJpjQ/34/dataset.csv",
    "https://datawrapper.dwcdn.net/bGS4P/29/dataset.csv",
    "https://datawrapper.dwcdn.net/Dp6Aq/14/dataset.csv",
    "https://datawrapper.dwcdn.net/wqjGw/5/dataset.csv"
  )

  organizations <- data.frame()

  for (url in urls) {
    df <- tryCatch({
      suppressWarnings({
        result <- readr::read_delim(url, delim = "\t", col_types = readr::cols(.default = "c"))
        problems <- readr::problems(result)
        if (nrow(problems) > 0) {
          warning(paste("Parsing issues in", url, "- Some data may be incorrect or missing."))
        }
        result
      })
    }, error = function(e) {
      warning(paste("Failed to read data from", url, ":", e$message))
      return(NULL)
    })

    if (!is.null(df) && ncol(df) >= 4) {
      df <- df[, 1:4]  # Keep only the first four columns
      names(df) <- c("Category", "Organization", "WMS_URL", "WFS_URL")  # Ensure consistent column names
      organizations <- rbind(organizations, df)
    }
  }

  if (nrow(organizations) == 0) {
    stop("Failed to retrieve organization data from all sources.")
  }

  # Clean URLs (remove brackets and parentheses, trim whitespace)
  clean_url <- function(url) {
    url <- gsub("\\[WMS\\]|\\[WFS\\]", "", url)
    url <- gsub("[()]", "", url)
    url <- trimws(url)
    return(url)
  }

  organizations$WMS_URL <- sapply(organizations$WMS_URL, clean_url)
  organizations$WFS_URL <- sapply(organizations$WFS_URL, clean_url)

  # Filter rows where WFS_URL is not empty or NA
  organizations <- organizations[organizations$WFS_URL != "" & !is.na(organizations$WFS_URL), ]

  # Concatenate Category and Organization into a new 'Name' column
  organizations$Name <- paste(organizations$Category, organizations$Organization, sep = " - ")

  organizations <- organizations[, c("Name", "WMS_URL", "WFS_URL")]

  return(organizations)
}
