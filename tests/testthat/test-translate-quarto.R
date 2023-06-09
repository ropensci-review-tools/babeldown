test_that("deepl_translate_quarto() works", {
  temp_dir <- withr::local_tempdir()
  babelquarto::quarto_multilingual_book(
    parent_dir = temp_dir,
    book_dir = "blop",
    main_language = "en",
    further_languages = "es"
  )
  book_path <- file.path(temp_dir, "blop")
  with_mock_dir("quarto-intro", {
    deepl_translate_quarto(
      book_path = book_path,
      chapter = "intro.qmd",
      force = TRUE, # the existing chapter is a placeholder
      render = FALSE,
      source_lang = "EN",
      target_lang = "ES",
      formality = "less"
    )
  })
  # have a look at the translation
  expect_match(readLines(file.path(book_path, "intro.es.qmd")), "libro", all = FALSE)
})
