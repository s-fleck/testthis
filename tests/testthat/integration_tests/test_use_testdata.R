context("use_testdata")


test_that("use_testdata works as expected", {
  package_state <- list.files(".", recursive = TRUE)

  expect_message(use_testdata(iris), "Saving to")
  expect_true(file.exists(rprojroot::find_testthat_root_file("testdata", "iris.rds")))

  tdat <- read_testdata("iris.rds")
  expect_identical(iris, tdat)

  unlink(rprojroot::find_testthat_root_file("testdata", "iris.rds"))
  expect_false(file.exists(rprojroot::find_testthat_root_file("testdata", "iris.rds")))

  # Check if tests did not modify package
  expect_identical(list.files(".", recursive = TRUE), package_state)
})
