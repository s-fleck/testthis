context("infrastructure-non cran tests")


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




test_that("use_testdata creates testdata dir", {
  #* @testing has_testdata
  #* @testing use_testdata
  package_state <- list.files(proj_test, recursive = TRUE)


  # Check for clean state
  expect_false(has_testdata())

  # Check creation of testdata dir
  expect_message(
    use_testdata(iris),
    "You can save data files for tests via"
  )

  expect_true(dir.exists(file.path(proj_test, "tests/testthat/testdata")))
  expect_true(has_testdata())

  # Cleanup
  unlink(
    file.path(proj_test, "tests/testthat/testdata"),
    recursive = TRUE
  )
  expect_false(has_testdata())


  # Check creation of testdata dfiles
  expect_output(
    expect_message(
      use_testdata(iris),
      "You can save data files for tests via"
    ),
    "Creating"
  )

  expect_output(
    expect_message(
      use_testdata(iris, subdir = "iris"),
      "You can save data files for tests via"
    ),
    "Creating"
  )

  expect_true(
    file.exists(file.path(proj_test, "tests/testthat/testdata/iris.rds"))
  )

  expect_true(
    file.exists(file.path(proj_test, "tests/testthat/testdata/iris/iris.rds"))
  )

  # Cleanup
  unlink(
    file.path(proj_test, "tests/testthat/testdata"),
    recursive = TRUE
  )

  # Check for unexpected changes to package
  package_state_new <- list.files(proj_test, recursive = TRUE)
  expect_identical(package_state, package_state_new)
})




test_that("use_tester works as expected", {
  package_state <- list.files(proj_test, recursive = TRUE)

  expect_true(dir.exists(proj_test))
  expect_true(dir.exists(file.path(proj_test, "R")))

  # Check if tester file is created at the correct path and not empty
  expect_message(expect_true(
    use_tester("footests", ignore = FALSE)),
    "creating tester function test_footests()"
  )

  efile <- file.path(proj_test, "R", "testthis-testers.R")

  expect_true(file.exists(efile))
  expect_true(file.size(efile) > 10)

  # cleanup
  unlink(efile)
  expect_false(file.exists(efile))


  # Check for unexpected changes to package
  package_state_new <- list.files(proj_test, recursive = TRUE)
  expect_identical(package_state, package_state_new)
})




test_that("use_test_subdir works as expected", {
  #* @testing use_test_subdir
  #* @testing use_acceptance_tests
  #* @testing use_integration_tests
  #* @testing use_manual_tests
  package_state <- list.files(proj_test, recursive = TRUE)

  # Check if tester file is created at the correct path and not empty
  expect_output(
  expect_message(expect_true(
    use_test_subdir("footests", ignore_tester = FALSE)),
    "creating tester function test_footests()"
  ),
    "Creating"
  )
  edir  <- file.path(proj_test, "tests", "testthat", "footests")
  efile <- file.path(proj_test, "R", "testthis-testers.R")

  expect_true(dir.exists(edir))
  expect_true(file.exists(efile))
  expect_true(file.size(efile) > 10)
  unlink(efile)
  unlink(edir, recursive = TRUE)


  # Check if preset subdir creators work
  edir <- file.path(proj_test, "tests/testthat/integration_tests/")
  expect_output(expect_true(use_integration_tests()), "Creating")
  expect_true(file.exists(edir))
  unlink(edir, recursive = TRUE)

  edir <- file.path(proj_test, "tests/testthat/acceptance_tests/")
  expect_output(expect_true(use_acceptance_tests()), "Creating")
  expect_true(file.exists(file.path(proj_test, "tests/testthat/acceptance_tests/")))
  unlink(edir, recursive = TRUE)

  edir <- file.path(proj_test, "tests/testthat/manual_tests/")
  expect_output(expect_true(use_manual_tests()), "Creating")
  expect_true(file.exists(edir))
  unlink(edir, recursive = TRUE)


  # Check for unexpected changes to package
  expect_identical(package_state, list.files(proj_test, recursive = TRUE))
})
