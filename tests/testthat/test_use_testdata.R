context("use_testdata")


test_that("use_testdata works as expected", {
  expect_message(use_testdata(iris), "Saving to")
  expect_true(file.exists("testdata/iris.rds"))

  tdat <- read_testdata("iris.rds")
  expect_identical(iris, tdat)

  unlink("testdata/iris.rds")
  expect_false(file.exists("testdata/iris.rds"))
})
