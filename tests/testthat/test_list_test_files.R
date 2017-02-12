context("list_test_files")


test_that("list_test_files works as expected", {
  #* @testing list_test_files
  #* @testing skip_test_files

  pkg  <- devtools::as.package(base::system.file(package = 'testthis'))
  tdir <- file.path(pkg, 'tests', 'testthat', 'test_data')

  # List all test files in /test/test_that
    tres <- with_mock(
      `devtools::as.package` = function(...) {pkg},
      `base::file.path`      = function(...) {tdir},
      list_test_files('.')
    )
    expect_identical(
      tres,
      list.files(tdir, full.names = TRUE)
    )


  # List all test files in /test/test_that, except the ones that contain the
  # testthis tag @skip
  tres <- with_mock(
    `devtools::as.package` = function(...) {pkg},
    `base::file.path`      = function(...) {tdir},
    list_test_files('.', skip = TRUE)
  )

  eres <- list.files(tdir, full.names = TRUE)
  eres <- eres[!grepl('testthis_tags.R', eres, fixed = TRUE)]  # file that contains @skip

  expect_identical(
    tres,
    eres
  )
})
