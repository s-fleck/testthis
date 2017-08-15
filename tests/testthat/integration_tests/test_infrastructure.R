context("infrastructure-non cran tests")




test_that("use_tester works as expected", {
  package_state <- list.files(".", recursive = TRUE)

  #* @testing use_tester
  tpkg <- rprojroot::find_testthat_root_file("testdata", "test_pkg")

  # Check if tester file is created at the correct path and not empty
  expect_message(expect_true(
    use_tester("footests", pkg = tpkg)),
    "creating tester function test_footests()"
  )
  efile <- file.path(tpkg, "R", "testthis-testers.R")

  expect_true(file.exists(efile))
  expect_true(file.size(efile) > 10)

  # cleanup
  unlink(efile)
  expect_false(file.exists(efile))


  # Check for unexpected changes to package
  package_state_new <- list.files(".", recursive = TRUE)
  expect_identical(package_state, package_state_new)
})




test_that("use_test_subdir works as expected", {
  package_state <- list.files(".", recursive = TRUE)
  tpkg <- rprojroot::find_testthat_root_file("testdata", "test_pkg")

  # Check if tester file is created at the correct path and not empty
  expect_message(expect_true(
    use_test_subdir("footests", pkg = tpkg)),
    "creating tester function test_footests()"
  )
  edir  <- file.path(tpkg, "tests", "testthat", "footests")
  efile <- file.path(tpkg, "R", "testthis-testers.R")

  expect_true(dir.exists(edir))
  expect_true(file.exists(efile))
  expect_true(file.size(efile) > 10)

  # cleanup
  unlink(efile)
  unlink(edir, recursive = TRUE)
  expect_false(file.exists(efile))
  expect_false(file.exists(edir))

  # Check for unexpected changes to package
  package_state_new <- list.files(".", recursive = TRUE)
  expect_identical(package_state, package_state_new)
})
