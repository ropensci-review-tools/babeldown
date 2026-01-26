test_that("get_glossary_id() works for an existing glossary", {
  vcr::local_cassette("get-glossary-ok")
  expect_equal(get_glossary_id("glosario", "en", "es"), "42")
})

test_that("get_glossary_id() returns NULL for absent glossary", {
  vcr::local_cassette("get-glossary-ok")
  expect_null(get_glossary_id("non-existing-glossary", "en", "es"))
})

test_that("get_glossary_id() errors if multiple glossaries with name", {
  vcr::local_cassette("get-glossary-multiple")
  expect_error(get_glossary_id("glosario", "en", "es"), "There are")
})

# test_that("can create glossary", {
#   with_mock_dir("glossary-creation", {
#    glossary <- deepl_upsert_glossary(
#       system.file("example-es-en.csv", package = "babeldown"),
#       glossary_name = "rstats-glosario",
#       target_lang = "Spanish",
#       source_lang = "English"
#     )
#    expect_equal(glossary, "42")
#   })
# })
