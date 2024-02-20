test_that("deepl_update() works", {

  dir <- withr::local_tempdir()
  print(dir)
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
  gert::git_add(c(fs::path_rel(file, dir), fs::path_rel(out_file, dir)), repo = dir)
  gert::git_commit_all("First commit", repo = dir)

  brio::write_lines(
    c("# a title", "", "this is some text", "", "awesome", "", "## subtitle", ""),
    file
  )
  gert::git_add(fs::path_rel(file, dir), repo = dir)
  gert::git_commit("Second commit", repo = dir)

  original_translation <- brio::read_lines(out_file)

  expect_snapshot(gert::git_ls(repo = dir)[,1])

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

test_that("deepl_update() works -- files in subdirectory", {

  dir <- withr::local_tempdir()
  fs::dir_create(file.path(dir, "pof"))
  file <- file.path(dir, "pof", "bla.md")
  fs::file_create(file)
  out_file <- file.path(dir, "pof", "bla.es.md")

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
  gert::git_add(c(fs::path_rel(file, dir), fs::path_rel(out_file, dir)), repo = dir)
  gert::git_commit_all("First commit", repo = dir)

  brio::write_lines(
    c("# a title", "", "this is some text", "", "awesome", "", "## subtitle", ""),
    file
  )
  gert::git_add(fs::path_rel(file, dir), repo = dir)
  gert::git_commit("Second commit", repo = dir)

  original_translation <- brio::read_lines(out_file)

  expect_snapshot(gert::git_ls(repo = dir)[,1])

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
