context("use_testdata")



test_that("test_parse_testthis_comments works as expected", {
  x <- read_testdata("read_testdata_testfile.rds")
  expect_identical(x, "185")
})



setup({
  tenv <- parent.env(environment())
  proj_old <- tryCatch(usethis::proj_get(), error = function(e) NULL)
  assign("proj_old", proj_old, tenv)
  assign("td", file.path(tempdir(), "testthis"), tenv)
  assign("proj_test", file.path(td, "test_pkg"), tenv)

  dir <- find_testdata("test_pkg", must_exist = TRUE)
  fs::dir_copy(dir, proj_test)
  usethis::proj_set(proj_test)
})




teardown({
  usethis::proj_set(proj_old)
  unlink(td, recursive = TRUE)
})




test_that("saving and loading testdata works", {
  # Check for clean state
  package_state <- list.files(proj_test, recursive = TRUE)
  efile <- file.path(proj_test, "tests", "testthat", "testdata", "iris.rds")


  # Test saving and loading
  expect_message(use_testdata(iris), "Saving to")
  expect_true(file.exists(efile))
  tdat <- read_testdata("iris.rds")
  expect_identical(iris, tdat)

  unlink(
    file.path(proj_test, "tests/testthat/testdata"),
    recursive = TRUE
  )

  # Check if tests did not modify package
  expect_identical(list.files(proj_test, recursive = TRUE), package_state)
})




test_that("use_testdata_raw works", {
  package_state <- list.files(proj_test, recursive = TRUE)
  efile <- file.path(proj_test, "tests", "testthat", "testdata-raw")

  # Test saving and loading
  use_testdata_raw()
  expect_true(dir.exists(efile))
  unlink(efile, recursive = TRUE)

  # Check if tests did not modify package
  expect_identical(list.files(proj_test, recursive = TRUE), package_state)
})
