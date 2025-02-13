test_that("argentum_interactive_download works with single layer selection", {
  # Set up mock responses in sequence
  responses <- c("1", "1", "1", "", "n")
  response_index <- 0

  # Mock readline to return responses in sequence
  mock_readline <- function(...) {
    response_index <<- response_index + 1
    responses[response_index]
  }

  testthat::with_mocked_bindings(
    {
      testthat::with_mocked_bindings(
        {
          results <- argentum_interactive_download()
          expect_s3_class(results, "data.frame")
          expect_equal(nrow(results), 1)
          expect_equal(results$status, "success")
        },
        readline = mock_readline,
        .package = "base"
      )
    },
    argentum_list_organizations = function(...) {
      data.frame(
        Name = c("Test Org 1", "Test Org 2"),
        WMS_URL = c("http://test1.com/wms", "http://test2.com/wms"),
        WFS_URL = c("http://test1.com/wfs", "http://test2.com/wfs"),
        stringsAsFactors = FALSE
      )
    },
    argentum_list_layers = function(...) {
      data.frame(
        Name = c("layer1", "layer2"),
        Title = c("Test Layer 1", "Test Layer 2"),
        stringsAsFactors = FALSE
      )
    },
    argentum_download_layers = function(...) {
      data.frame(
        layer_name = "layer1",
        status = "success",
        file_path = "wfs_layers/layer1.gpkg",
        error_message = NA_character_,
        stringsAsFactors = FALSE
      )
    },
    .package = "Argentum"
  )
})

test_that("argentum_interactive_download handles invalid organization selection", {
  testthat::with_mocked_bindings(
    {
      testthat::with_mocked_bindings(
        {
          expect_error(
            argentum_interactive_download(),
            "Invalid organization selection"
          )
        },
        readline = function(...) "999",
        .package = "base"
      )
    },
    argentum_list_organizations = function(...) {
      data.frame(
        Name = c("Test Org 1", "Test Org 2"),
        WMS_URL = c("http://test1.com/wms", "http://test2.com/wms"),
        WFS_URL = c("http://test1.com/wfs", "http://test2.com/wfs"),
        stringsAsFactors = FALSE
      )
    },
    .package = "Argentum"
  )
})

test_that("argentum_interactive_download handles invalid layer selection", {
  responses <- c("1", "999", "1", "", "n")
  response_index <- 0

  mock_readline <- function(...) {
    response_index <<- response_index + 1
    responses[response_index]
  }

  testthat::with_mocked_bindings(
    {
      testthat::with_mocked_bindings(
        {
          expect_error(
            argentum_interactive_download(),
            "Invalid layer selection"
          )
        },
        readline = mock_readline,
        .package = "base"
      )
    },
    argentum_list_organizations = function(...) {
      data.frame(
        Name = c("Test Org 1", "Test Org 2"),
        WMS_URL = c("http://test1.com/wms", "http://test2.com/wms"),
        WFS_URL = c("http://test1.com/wfs", "http://test2.com/wfs"),
        stringsAsFactors = FALSE
      )
    },
    argentum_list_layers = function(...) {
      data.frame(
        Name = c("layer1", "layer2"),
        Title = c("Test Layer 1", "Test Layer 2"),
        stringsAsFactors = FALSE
      )
    },
    .package = "Argentum"
  )
})
