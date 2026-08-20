test_that("deprecated wrappers warn once per session and keep 1.x column names", {
  local_quiet()
  with_fake_catalog({
    rm(list = ls(Argentum:::.arg_warned), envir = Argentum:::.arg_warned)

    expect_warning(out <- argentum_list_organizations(), class = "argentum_deprecated")
    expect_named(out, c("Name", "WMS_URL", "WFS_URL"))

    # Second call is silent
    expect_silent(argentum_list_organizations())
  })
})

test_that("the deprecation message names the replacement", {
  rm(list = ls(Argentum:::.arg_warned), envir = Argentum:::.arg_warned)
  expect_warning(
    tryCatch(argentum_import_wfs_layer("https://x/ows", "l"), error = function(e) NULL),
    "argentum_read_wfs"
  )
})

test_that("select_organization still honours non-interactive mode", {
  local_quiet()
  with_fake_catalog({
    rm(list = ls(Argentum:::.arg_warned), envir = Argentum:::.arg_warned)
    out <- suppressWarnings(argentum_select_organization(interactive_select = FALSE))
    expect_equal(nrow(out), 1L)
  })
})
