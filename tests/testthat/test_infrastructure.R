context("infrastructure")


test_that("use_testdata creates testdata dir", {
  #* @testing testdata_exists
  #* @testing use_testdata

  tpkg <- file.path(testthat::test_path(), "testdata/test_pkg")

  expect_false(testdata_exists(tpkg))

  expect_message(
    use_testdata(pkg = tpkg),
    "You can save data files for tests via"
  )

  expect_true(dir.exists(file.path(tpkg, "tests/testthat/testdata")))
  expect_true(testdata_exists(pkg = tpkg))

  unlink(
    file.path(tpkg, "tests/testthat/testdata"),
    recursive = TRUE
  )

  expect_false(testdata_exists(tpkg))
})
