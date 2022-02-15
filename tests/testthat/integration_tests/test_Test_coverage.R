test_that("test_coverage works", {
  expect_silent(test_coverage())
  expect_silent(test_coverage(from_tags = FALSE))
  expect_silent(test_coverage(from_desc = FALSE))
})
