context("infrastructure")


test_that("infrastructure works as expected", {
  tpkg <- 'tests/testthat/testdata/test_pkg/'

  expect_message(
    use_testdata(iris, cars, pkg = tpkg), 'Saving iris.rds, cars.rds '
  )


  expect_true(dir.exists('tests/testthat/testdata/test_pkg/tests/testthat/testdata'))
  expect_true(file.exists('tests/testthat/testdata/test_pkg/tests/testthat/testdata/iris.rds'))
  expect_true(file.exists('tests/testthat/testdata/test_pkg/tests/testthat/testdata/cars.rds'))

  expect_error(
    use_testdata(iris, cars, pkg = tpkg),
    "iris.rds, cars.rds already exist in"
  )

  unlink(
    'tests/testthat/testdata/test_pkg/tests/testthat/testdata',
    recursive = TRUE
  )

})
