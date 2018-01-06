context("use_testdata")

proj <- rprojroot::find_package_root_file()


test_that("saving and loading testdata works", {
  #* @testing read_testdata
  #* @testing use_testdata

  # Check for clean state
  package_state <- list.files(".", recursive = TRUE)
  tpkg <- file.path(rprojroot::find_testthat_root_file("testdata", "test_pkg"))
  usethis::proj_set(tpkg)
  efile <- file.path(tpkg, "tests", "testthat", "testdata", "iris.rds")


  # Test saving and loading
  expect_output(expect_message(use_testdata(iris), "Saving to"))
  expect_true(file.exists(efile))
  tdat <- read_testdata("iris.rds")
  expect_identical(iris, tdat)

  unlink(
    file.path(tpkg, "tests/testthat/testdata"),
    recursive = TRUE
  )

  # Check if tests did not modify package
  usethis::proj_set(proj)
  expect_identical(list.files(".", recursive = TRUE), package_state)
})


test_that("use_testdata_raw works", {
  #* @testing use_testdata_raw

  package_state <- list.files(".", recursive = TRUE)
  tpkg <- file.path(rprojroot::find_testthat_root_file("testdata", "test_pkg"))
  usethis::proj_set(tpkg)
  efile <- file.path(tpkg, "tests", "testthat", "testdata-raw")


  # Test saving and loading
  expect_output(use_testdata_raw(), "Creating")
  expect_true(dir.exists(efile))
  unlink(efile, recursive = TRUE)

  # Check if tests did not modify package
  usethis::proj_set(proj)
  expect_identical(list.files(".", recursive = TRUE), package_state)
})


usethis::proj_set(proj)
