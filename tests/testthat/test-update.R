test_that("deepl_update() works", {

  dir <- withr::local_tempdir()
  file <- file.path(dir, "bla.md")
  fs::file_create(file)
  out_file <- file.path(dir, "bla.es.md")

  brio::write_lines(
    c("# header", "", "this is some text", "## subtitle", "", "nice!"),
    file
  )

  with_mock_dir("git1", {
    deepl_translate(
      path = file,
      out_path = out_file,
      source_lang = "EN",
      target_lang = "ES",
      formality = "less",
      yaml_fields = NULL
    )
  })

  gert::git_init(dir)
  gert::git_config_set("user.name", "Jane Doe", repo = dir)
  gert::git_config_set("user.email", "jane@example.com", repo = dir)
  gert::git_add(c(fs::path_file(file), fs::path_file(out_file)), repo = dir)
  gert::git_commit_all("First commit", repo = dir)

  brio::write_lines(
    c("# a title", "", "this is some text", "", "awesome", "", "## subtitle", ""),
    file
  )
  gert::git_add(fs::path_file(file), repo = dir)
  gert::git_commit("Second commit", repo = dir)

  original_translation <- brio::read_lines(out_file)

  with_mock_dir("git2", {
    deepl_update(
      path = file,
      out_path = out_file,
      source_lang = "EN",
      target_lang = "ES",
      formality = "less",
      yaml_fields = NULL
    )
  })

  new_translation <- brio::read_lines(out_file)
  expect_true(
    !all(new_translation %in% original_translation)
  )
  expect_true(
    any(new_translation %in% original_translation)
  )

})
