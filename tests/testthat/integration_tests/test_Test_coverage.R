context("get_test_coverage")

proj <- usethis::proj_get()

test_that("test_Test_coverage works as expected", {
  tpkg <- rprojroot::find_testthat_root_file("testdata", "test_pkg")
  usethis::proj_set(tpkg)
  expect_error(dat <- get_test_coverage(), "Functions can only be detected for installed packages")
  usethis::proj_set(proj)
  expect_silent(dat <- get_test_coverage())
})

usethis::proj_set(proj)
