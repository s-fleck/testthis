context("list_test_files")

proj <- rprojroot::find_package_root_file()

test_that("list_test_files works as expected", {
  #* @testing list_test_files
  #* @testing skip_test_files

  tpkg <- rprojroot::find_testthat_root_file("testdata", "test_pkg")
  usethis::proj_set(tpkg)

  # List all test files in /test/test_that
    tres <- list_test_files()
    expect_identical(
      tres,
      list.files(file.path(tpkg, "tests", "testthat"), full.names = TRUE)
    )


  # List all test files in /test/test_that, except the ones that contain the
  # testthis tag @skip
  tres <- list_test_files(skip = TRUE)

  eres <- list.files(file.path(tpkg, "tests", "testthat"), full.names = TRUE)
  eres <- eres[!grepl('testthis_tags.R', eres, fixed = TRUE)]  # file that contains @skip

  expect_identical(
    tres,
    eres
  )

  usethis::proj_set(proj)
})

usethis::proj_set(proj)
