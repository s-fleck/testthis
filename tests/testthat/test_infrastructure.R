context("infrastructure")


test_that("use_testdata creates testdata dir", {
  #* @testing testdata_exists
  #* @testing use_testdata

  tpkg <- 'tests/testthat/testdata/test_pkg/'

  expect_false(testdata_exists(tpkg))

  expect_message(
    use_testdata(pkg = tpkg),
    "Creating `tests/testthat/testdata`"
  )

  expect_message(
    use_testdata(pkg = tpkg),
    "already exists"
  )

  message("* ", path, " already exists.")

  expect_true(dir.exists('tests/testthat/testdata/test_pkg/tests/testthat/testdata'))
  expect_true(testdata_exists())

  unlink(
    'tests/testthat/testdata/test_pkg/tests/testthat/testdata',
    recursive = TRUE
  )
})
