context("get_test_coverage")

test_that("test_Test_coverage works as expected", {
  expect_silent(dat <- get_test_coverage())
})
