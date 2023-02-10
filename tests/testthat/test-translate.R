test_that("deepl_translate() errors well", {
  expect_snapshot(
    deepl_translate("non-existing-file"),
    error = TRUE
  )

  file <- withr::local_tempfile()
  file.create(file)
  with_mock_dir("get-glossary-ok", {
    expect_snapshot(
      deepl_translate(file, glossary_name = "non-existing-glossary"),
      error = TRUE
    )
  })

  expect_snapshot(
    deepl_translate(file, formality = "non-existing-formality"),
    error = TRUE
  )
})
