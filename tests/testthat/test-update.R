test_that("deepl_update() works", {
  dir <- withr::local_tempdir()

  file <- file.path(dir, "bla.md")
  fs::file_create(file)
  out_file <- file.path(dir, "bla.es.md")

  brio::write_lines(
    c("# header", "", "this is some text", "## subtitle", "", "nice!"),
    file
  )

  vcr::local_cassette("git1")
  deepl_translate(
    path = file,
    out_path = out_file,
    source_lang = "EN",
    target_lang = "ES",
    formality = "less",
    yaml_fields = NULL
  )

  gert::git_init(dir)
  gert::git_config_set("user.name", "Jane Doe", repo = dir)
  gert::git_config_set("user.email", "jane@example.com", repo = dir)
  gert::git_add(
    c(fs::path_rel(file, dir), fs::path_rel(out_file, dir)),
    repo = dir
  )
  gert::git_commit_all("First commit", repo = dir)

  brio::write_lines(
    c(
      "# a title",
      "",
      "this is some text",
      "",
      "awesome",
      "",
      "## subtitle",
      ""
    ),
    file
  )
  gert::git_add(fs::path_rel(file, dir), repo = dir)
  gert::git_commit("Second commit", repo = dir)

  original_translation <- brio::read_lines(out_file)

  expect_snapshot(gert::git_ls(repo = dir)[, 1])

  vcr::local_cassette("git2")
  deepl_update(
    path = file,
    out_path = out_file,
    source_lang = "EN",
    target_lang = "ES",
    formality = "less",
    yaml_fields = NULL
  )

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

  vcr::local_cassette("git1")
  deepl_translate(
    path = file,
    out_path = out_file,
    source_lang = "EN",
    target_lang = "ES",
    formality = "less",
    yaml_fields = NULL
  )

  gert::git_init(dir)
  gert::git_config_set("user.name", "Jane Doe", repo = dir)
  gert::git_config_set("user.email", "jane@example.com", repo = dir)
  gert::git_add(
    c(fs::path_rel(file, dir), fs::path_rel(out_file, dir)),
    repo = dir
  )
  gert::git_commit_all("First commit", repo = dir)

  brio::write_lines(
    c(
      "# a title",
      "",
      "this is some text",
      "",
      "awesome",
      "",
      "## subtitle",
      ""
    ),
    file
  )
  gert::git_add(fs::path_rel(file, dir), repo = dir)
  gert::git_commit("Second commit", repo = dir)

  original_translation <- brio::read_lines(out_file)

  expect_snapshot(gert::git_ls(repo = dir)[, 1])

  vcr::local_cassette("git2")
  deepl_update(
    path = file,
    out_path = out_file,
    source_lang = "EN",
    target_lang = "ES",
    formality = "less",
    yaml_fields = NULL
  )

  new_translation <- brio::read_lines(out_file)
  expect_true(
    !all(new_translation %in% original_translation)
  )
  expect_true(
    any(new_translation %in% original_translation)
  )
})

test_that("deepl_branch_update() works", {
  dir <- withr::local_tempdir()

  # Copy template repository
  fs::dir_copy(
    test_path("templates", "repo-template"),
    dir,
    overwrite = TRUE
  )

  # Initialize git repository
  gert::git_init(dir)
  gert::git_config_set("user.name", "Jane Doe", repo = dir)
  gert::git_config_set("user.email", "jane@example.com", repo = dir)

  # Initial commit on main branch
  gert::git_add(".", repo = dir)
  gert::git_commit("Initial commit with translations", repo = dir)

  # Create a new branch
  gert::git_branch_create("update-content", repo = dir)
  gert::git_branch_checkout("update-content", repo = dir)

  # Modify the English source file
  file.copy(
    test_path("templates", "index-updated.qmd"),
    file.path(dir, "index.qmd"),
    overwrite = TRUE
  )

  # Commit the changes
  gert::git_add("index.qmd", repo = dir)
  gert::git_commit("Update English content", repo = dir)

  # Run deepl_branch_update
  vcr::local_cassette("branch-update")
  deepl_branch_update(repo = dir)

  # Check that translations were updated
  expect_snapshot_file(file.path(dir, "index.es.qmd"), "index.es.qmd")
  expect_snapshot_file(file.path(dir, "index.fr.qmd"), "index.fr.qmd")
})
