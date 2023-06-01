test_that("deepl_translate_quarto() works", {
  skip_if_offline()
  temp_dir <- withr::local_tempdir()
  if (!blogdown::hugo_available()) {
    blogdown::install_hugo('0.111.3')
  }
  blogdown::new_site(dir = temp_dir)
  post_path <- file.path(temp_dir, "content", "post", "2016-12-30-hello-markdown", "index.md")
  with_mock_dir("hugo-intro", {
  deepl_translate_hugo(
    post_path = post_path,
    force = TRUE, # the existing chapter is a placeholder
    source_lang = "EN",
    target_lang = "FR",
    formality = "less"
  )
  })
  target_path <- file.path(temp_dir, "content", "post", "2016-12-30-hello-markdown", "index.fr.md")
  expect_true(fs::file_exists(target_path))
  # have a look at the translation
  expect_match(readLines(target_path), "syntaxe Markdown spéciale", all = FALSE)

  yaml <- rmarkdown::yaml_front_matter(target_path)
  expect_match(yaml[["slug"]], "billet")
})
