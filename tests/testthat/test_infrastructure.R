context("infrastructure")




test_that("use_testdata creates testdata dir", {
  #* @testing has_testdata
  #* @testing use_testdata
  package_state <- list.files(".", recursive = TRUE)

  tpkg <- file.path(rprojroot::find_testthat_root_file("testdata", "test_pkg"))

  # Check for clean state
  expect_false(has_testdata(tpkg))

  # Check creation of testdata dir
  expect_output(
    expect_message(
      use_testdata(iris, base_path = tpkg),
      "You can save data files for tests via"
    ),
    "Creating"
  )

  expect_true(dir.exists(file.path(tpkg, "tests/testthat/testdata")))
  expect_true(has_testdata(base_path = tpkg))

    # Cleanup
    unlink(
      file.path(tpkg, "tests/testthat/testdata"),
      recursive = TRUE
    )
    expect_false(has_testdata(tpkg))


  # Check creation of testdata dfiles
    expect_output(
      expect_message(
        use_testdata(iris, base_path = tpkg),
        "You can save data files for tests via"
      ),
        "Creating"
    )

    expect_output(
      expect_message(
        use_testdata(iris, subdir = "iris", base_path = tpkg),
        "You can save data files for tests via"
      ),
        "Creating"
    )

    expect_true(file.exists(file.path(tpkg, "tests/testthat/testdata/iris.rds")))
    expect_true(file.exists(file.path(tpkg, "tests/testthat/testdata/iris/iris.rds")))

    # Cleanup
    unlink(
      file.path(tpkg, "tests/testthat/testdata"),
      recursive = TRUE
    )

  # Check for unexpected changes to package
  package_state_new <- list.files(".", recursive = TRUE)
  expect_identical(package_state, package_state_new)
})
