test_that("examine_source_lang() works", {
  vcr::local_cassette("langs")
  expect_equal(examine_source_lang("en"), "EN")
  expect_equal(examine_source_lang("english"), "EN")
  expect_snapshot(examine_source_lang("englisssh"), error = TRUE)
})

test_that("examine_target_lang() works", {
  vcr::local_cassette("langs-target")
  expect_equal(examine_target_lang("en-us"), "EN-US")
  expect_equal(examine_target_lang("english (american)"), "EN-US")
  expect_equal(examine_target_lang("English (American)"), "EN-US")
  expect_snapshot(examine_target_lang("English"), error = TRUE)
})
