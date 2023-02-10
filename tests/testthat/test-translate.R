test_that("deepl_translate() works well", {
  with_mock_dir("example", {
    to_translate <- system.file("example.md", package = "babeldown")
    out_path <- withr::local_tempfile()

    deepl_translate(
      path = to_translate,
      out_path = out_path,
      source_lang = "EN",
      target_lang = "ES",
      formality = "less"
    )

    expect_true(file.exists(out_path))
    translated_lines <- readLines(out_path)
    expect_true(any(grepl("paquete", translated_lines)))
  })

})

test_that("deepl_translate() errors well", {
  expect_snapshot(
    deepl_translate("non-existing-file"),
    error = TRUE
  )

  file <- withr::local_tempfile()
  file.create(file)
  with_mock_dir("get-glossary-ok", {
    expect_snapshot(
      deepl_translate(
        file,
        glossary_name = "non-existing-glossary",
        source_lang = "en",
        target_lang = "es"
      ),
      error = TRUE
    )
  })

  expect_snapshot(
    deepl_translate(
      file,
      formality = "non-existing-formality",
      source_lang = "en",
      target_lang = "es"
    ),
    error = TRUE
  )
})
