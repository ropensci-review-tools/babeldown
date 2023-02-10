test_that("deepl_usage() works", {
  with_mock_dir("usage", {
    usage <- deepl_usage()
    expect_type(usage, "list")
    expect_named(usage, c("character_count", "character_limit"))
  })
})
