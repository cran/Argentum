# Hardened XML parsing. What these tests pin down is documented at the top of
# R/xmlsafe.R: neither XXE nor billion laughs works against xml2's defaults,
# but both defences depend on defaults staying defaults. This file is what
# turns that from an accident into a contract.

test_that("entity declarations are rejected", {
  bomb <- paste0(
    '<?xml version="1.0"?>',
    '<!DOCTYPE lolz [',
    '<!ENTITY lol "lol"><!ENTITY lol2 "&lol;&lol;&lol;&lol;&lol;&lol;">',
    ']><lolz>&lol2;</lolz>'
  )
  expect_error(arg_read_xml(bomb), class = "argentum_error_xml")

  # Case and internal whitespace do not dodge the check.
  expect_error(
    arg_read_xml('<?xml version="1.0"?><!DOCTYPE x [<! entity e "v">]><x/>'),
    class = "argentum_error_xml"
  )
})

test_that("an external entity declaration is rejected before parsing", {
  xxe <- paste0(
    '<?xml version="1.0"?>',
    '<!DOCTYPE x [<!ENTITY xxe SYSTEM "file:///etc/hostname">]>',
    '<x>&xxe;</x>'
  )
  expect_error(arg_read_xml(xxe), class = "argentum_error_xml")
})

test_that("a bare DOCTYPE without entities still parses", {
  # WFS 1.0.0 / WMS 1.1.1 capabilities legitimately declare the OGC DTD.
  doc <- paste0(
    '<?xml version="1.0"?>',
    '<!DOCTYPE WMT_MS_Capabilities SYSTEM ',
    '"http://schemas.opengis.net/wms/1.1.1/capabilities_1_1_1.dtd">',
    '<WMT_MS_Capabilities version="1.1.1"><Service><Title>ok</Title></Service>',
    '</WMT_MS_Capabilities>'
  )
  parsed <- arg_read_xml(doc)
  expect_s3_class(parsed, "xml_document")
  expect_equal(xml2::xml_attr(xml2::xml_root(parsed), "version"), "1.1.1")
})

test_that("a normal capabilities document does not trip the detection", {
  doc <- '<WFS_Capabilities version="2.0.0"><FeatureTypeList/></WFS_Capabilities>'
  expect_s3_class(arg_read_xml(doc), "xml_document")
})

test_that("oversized documents are rejected", {
  expect_error(
    arg_read_xml("<x>abc</x>", max_bytes = 5),
    class = "argentum_error_xml"
  )
  # The cap is measured in bytes, not characters.
  expect_error(
    arg_read_xml("<x>áááá</x>", max_bytes = 10),
    class = "argentum_error_xml"
  )
})

test_that("broken XML raises the package's own error class", {
  expect_error(arg_read_xml("this is not xml"), class = "argentum_error_xml")
  expect_error(arg_read_xml(""), class = "argentum_error_xml")
})
