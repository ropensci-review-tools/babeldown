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

test_that("deepl_translate() does not translate Markdown blocks (#16)", {
  with_mock_dir("example-markdown-blocks", {
    to_translate <- system.file("example-markdown-blocks.md", package = "babeldown")
    out_path <- withr::local_tempfile()

    deepl_translate(
      path = to_translate,
      out_path = out_path,
      source_lang = "EN",
      target_lang = "ES",
      formality = "less"
    )

    link_not_translated <- any(grepl("very nice", readLines(out_path)))
    expect_true(link_not_translated)
  })

})

test_that("deepl_translate() can translate YAML field (#16)", {
  with_mock_dir("example-yaml", {
    to_translate <- system.file("example-yaml.md", package = "babeldown")
    out_path <- withr::local_tempfile()

    deepl_translate(
      path = to_translate,
      out_path = out_path,
      source_lang = "EN",
      target_lang = "ES",
      formality = "less",
      yaml_fields = c("title", "description", "slug")
    )

    lines <- readLines(out_path)
    expect_false(any(grepl("Wonderful", lines)))
    expect_false(any(grepl("Check", lines)))
    expect_false(any(grepl("universe", lines)))
  })

})
test_that("deepl_translate() can skip translation of YAML field (#16)", {
  with_mock_dir("example-yaml", {
    to_translate <- system.file("example-yaml.md", package = "babeldown")
    out_path <- withr::local_tempfile()

    deepl_translate(
      path = to_translate,
      out_path = out_path,
      source_lang = "EN",
      target_lang = "ES",
      formality = "less",
      yaml_fields = NULL
    )

    lines <- readLines(out_path)
    expect_true(any(grepl("Wonderful", lines)))
    expect_true(any(grepl("Check", lines)))
    expect_true(any(grepl("universe", lines)))
  })

})

