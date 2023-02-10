test_that("get_glossary_id() works for an existing glossary", {
  with_mock_dir("get-glossary-ok", {
    expect_equal(get_glossary_id("glosario"), "42")
  })
})

test_that("get_glossary_id() returns NULL for absent glossary", {
  with_mock_dir("get-glossary-ok", {
    expect_null(get_glossary_id("non-existing-glossary"))
  })
})

test_that("get_glossary_id() errors if multiple glossaries with name", {
  with_mock_dir("get-glossary-multiple", {
    expect_error(get_glossary_id("glosario"), "There are")
  })
})

test_that("can create glossary", {
  with_mock_dir("glossary-creation", {
   glossary <- deepl_upsert_glossary(
      system.file("example-es-en.csv", package = "babeldown"),
      glossary_name = "rstats-glosario",
      target_lang = "Spanish",
      source_lang = "English"
    )
   expect_equal(glossary, "42")
  })
})
