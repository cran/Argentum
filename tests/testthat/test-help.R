test_that("argentum_help prints the getting-started guide by default", {
  msgs <- paste(capture_messages(argentum_help()), collapse = "\n")
  expect_match(msgs, "argentum_organizations", fixed = TRUE)
  expect_match(msgs, "argentum_read_wfs", fixed = TRUE)
})

test_that("every topic prints something in both languages", {
  for (topic in c("inicio", "catalogo", "capas", "descargar", "wms", "opciones")) {
    for (lang in c("es", "en")) {
      msgs <- capture_messages(argentum_help(topic, lang = lang))
      expect_gt(length(msgs), 0)
    }
  }
})

test_that("English synonyms map to the same topics", {
  expect_identical(
    suppressMessages(argentum_help("download", lang = "en")),
    suppressMessages(argentum_help("descargar"))
  )
  expect_identical(
    suppressMessages(argentum_help("start")),
    suppressMessages(argentum_help("inicio"))
  )
})

test_that("an unknown topic fails with the package's own error class", {
  expect_error(argentum_help("mapas"), class = "argentum_error_help")
  expect_error(argentum_help(c("a", "b")), class = "argentum_error_help")
})

test_that("the topic is returned invisibly", {
  expect_invisible(suppressMessages(argentum_help("wms")))
  expect_identical(suppressMessages(argentum_help("catalog", lang = "en")), "catalogo")
})
