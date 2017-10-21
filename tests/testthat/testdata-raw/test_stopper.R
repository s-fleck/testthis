context("Tests in this file should never be run by test_* function")

test_that("this is not be run", {
  expect_silent(stop())
})

