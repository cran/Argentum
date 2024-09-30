library(testthat)
suppressWarnings(library(mockery))

# Ensure the Argentum package is installed and loaded
library(Argentum)

# Mock data
mock_org_data <- data.frame(
  Category = c("Cat1", "Cat2"),
  Organization = c("Org1", "Org2"),
  WMS_URL = c("http://wms1.com", "http://wms2.com"),
  WFS_URL = c("http://wfs1.com", "http://wfs2.com"),
  stringsAsFactors = FALSE
)

mock_layer_data <- data.frame(
  Name = c("Layer1", "Layer2"),
  Title = c("Layer One", "Layer Two"),
  stringsAsFactors = FALSE
)

# Test argentum_list_organizations
test_that("argentum_list_organizations returns correct data frame", {
  mock_read_delim <- function(file, delim, col_types) {
    if (grepl("nH8e7", file) || grepl("JJpjQ", file)) {
      mock_org_data
    } else {
      stop("Simulated error for other URLs")
    }
  }

  stub(argentum_list_organizations, "readr::read_delim", mock_read_delim)

  result <- suppressWarnings(argentum_list_organizations())
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 3)
  expect_equal(names(result), c("Name", "WMS_URL", "WFS_URL"))
  expect_equal(nrow(result), 4)  # Updated to expect 4 rows
  expect_true(all(result$WFS_URL != "" & !is.na(result$WFS_URL)))
  expect_equal(result$Name, rep(c("Cat1 - Org1", "Cat2 - Org2"), 2))  # Updated to expect repeated names
})

# Test argentum_select_organization
test_that("argentum_select_organization returns correct organization", {
  mock_menu <- mock(1)
  stub(argentum_select_organization, "menu", mock_menu)
  stub(argentum_select_organization, "argentum_list_organizations", function() {
    data.frame(
      Name = c("Cat1 - Org1", "Cat2 - Org2"),
      WMS_URL = c("http://wms1.com", "http://wms2.com"),
      WFS_URL = c("http://wfs1.com", "http://wfs2.com"),
      stringsAsFactors = FALSE
    )
  })

  # Test interactive mode
  result <- argentum_select_organization(interactive_select = TRUE)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(names(result), c("Name", "WMS_URL", "WFS_URL"))
  expect_equal(result$Name, "Cat1 - Org1")

  # Test non-interactive mode
  result_non_interactive <- argentum_select_organization(interactive_select = FALSE)
  expect_s3_class(result_non_interactive, "data.frame")
  expect_equal(nrow(result_non_interactive), 1)
  expect_equal(names(result_non_interactive), c("Name", "WMS_URL", "WFS_URL"))
  expect_equal(result_non_interactive$Name, "Cat1 - Org1")
})

# Test argentum_list_layers
test_that("argentum_list_layers handles errors correctly", {
  expect_error(argentum_list_layers(), "Please provide a valid organization name.")

  stub(argentum_list_layers, "argentum_list_organizations", function() {
    data.frame(
      Name = c("Cat1 - Org1", "Cat2 - Org2"),
      WMS_URL = c("http://wms1.com", "http://wms2.com"),
      WFS_URL = c("http://wfs1.com", "http://wfs2.com"),
      stringsAsFactors = FALSE
    )
  })
  expect_error(argentum_list_layers("NonExistentOrg"), "Organization not found.")
})

test_that("argentum_list_layers returns correct layer information", {
  stub(argentum_list_layers, "argentum_list_organizations", function() {
    data.frame(
      Name = c("Cat1 - Org1", "Cat2 - Org2"),
      WMS_URL = c("http://wms1.com", "http://wms2.com"),
      WFS_URL = c("http://wfs1.com", "http://wfs2.com"),
      stringsAsFactors = FALSE
    )
  })
  stub(argentum_list_layers, "argentum_get_capabilities", function(url) {
    xml2::read_xml('
      <WFS_Capabilities>
        <FeatureTypeList>
          <FeatureType>
            <Name>Layer1</Name>
            <Title>Layer One</Title>
          </FeatureType>
          <FeatureType>
            <Name>Layer2</Name>
            <Title>Layer Two</Title>
          </FeatureType>
        </FeatureTypeList>
      </WFS_Capabilities>
    ')
  })

  result <- argentum_list_layers("Cat1 - Org1")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(names(result), c("Name", "Title"))
  expect_equal(result$Name, c("Layer1", "Layer2"))
  expect_equal(result$Title, c("Layer One", "Layer Two"))
})

# Test argentum_import_wfs_layer
test_that("argentum_import_wfs_layer returns sf object", {
  mock_sf_object <- structure(list(geometry = structure(list(structure(c(0, 0), class = c("XY", "POINT", "sfg"))), class = c("sfc_POINT", "sfc"), precision = 0, bbox = structure(c(xmin = 0, ymin = 0, xmax = 0, ymax = 0), class = "bbox"), crs = structure(list(input = "EPSG:4326", wkt = "GEOGCRS[\"WGS 84\"]"), class = "crs"), n_empty = 0L)), class = c("sf", "data.frame"), sf_column = "geometry", agr = structure(integer(0), class = "factor", .Label = c("constant", "aggregate", "identity"), .Names = character(0)))

  stub(argentum_import_wfs_layer, "sf::read_sf", function(...) mock_sf_object)
  result <- argentum_import_wfs_layer("http://test.com/wfs", "TestLayer")
  expect_s3_class(result, "sf")
})

# Test argentum_interactive_import
test_that("argentum_interactive_import returns sf object", {
  mock_menu <- mock(1, cycle = TRUE)
  mock_readline <- mock("1", cycle = TRUE)
  mock_sf_object <- structure(list(geometry = structure(list(structure(c(0, 0), class = c("XY", "POINT", "sfg"))), class = c("sfc_POINT", "sfc"), precision = 0, bbox = structure(c(xmin = 0, ymin = 0, xmax = 0, ymax = 0), class = "bbox"), crs = structure(list(input = "EPSG:4326", wkt = "GEOGCRS[\"WGS 84\"]"), class = "crs"), n_empty = 0L)), class = c("sf", "data.frame"), sf_column = "geometry", agr = structure(integer(0), class = "factor", .Label = c("constant", "aggregate", "identity"), .Names = character(0)))

  stub(argentum_interactive_import, "menu", mock_menu)
  stub(argentum_interactive_import, "readline", mock_readline)
  stub(argentum_interactive_import, "argentum_list_organizations", function() {
    data.frame(
      Name = c("Cat1 - Org1", "Cat2 - Org2"),
      WMS_URL = c("http://wms1.com", "http://wms2.com"),
      WFS_URL = c("http://wfs1.com", "http://wfs2.com"),
      stringsAsFactors = FALSE
    )
  })
  stub(argentum_interactive_import, "argentum_list_layers", function(...) mock_layer_data)
  stub(argentum_interactive_import, "argentum_import_wfs_layer", function(...) mock_sf_object)

  result <- argentum_interactive_import()
  expect_s3_class(result, "sf")
})

# Tests for clean_url
test_that("clean_url removes whitespace and parentheses", {
  expect_equal(clean_url(" http://example.com "), "http://example.com")
  expect_equal(clean_url("http://example.com (some text)"), "http://example.com")
  expect_equal(clean_url(" http://example.com (some text) "), "http://example.com")
})

# Test argentum_get_capabilities
test_that("argentum_get_capabilities handles failed request", {
  mock_error_response <- structure(
    list(status_code = 500),
    class = c("response", "list")
  )

  mock_GET <- mockery::mock(mock_error_response, cycle = TRUE)
  mock_stop_for_status <- mockery::mock(stop("HTTP error 500"), cycle = TRUE)

  stub(argentum_get_capabilities, "Sys.sleep", function(x) NULL)

  local_mocked_bindings(
    GET = mock_GET,
    stop_for_status = mock_stop_for_status,
    .package = "httr"
  )

  expect_error(
    argentum_get_capabilities("http://example.com/wms"),
    "Failed to retrieve capabilities after 3 attempts: HTTP error 500"
  )

  # Check that GET was called at least 3 times
  expect_gte(length(mockery::mock_args(mock_GET)), 3)
  # Check that stop_for_status was called at least 3 times
  expect_gte(length(mockery::mock_args(mock_stop_for_status)), 3)
})
