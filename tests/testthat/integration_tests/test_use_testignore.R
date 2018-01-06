context("use_testignore")

proj <- rprojroot::find_package_root_file()

test_that("use_testignore works as expected", {
  fs   <- list.files(".")
  tpkg <- rprojroot::find_testthat_root_file("testdata", "test_pkg")
  usethis::proj_set(tpkg)
  ti   <- file.path(tpkg, "tests/testthat/_testignore")

  expect_message(use_testignore("blubb"))
  expect_true(file.exists(ti))
  tdat1 <- readLines(ti)

  expect_message(use_testignore("blubb2"))
  tdat2 <- readLines(ti)

  expect_false(identical(tdat1, tdat2))
  expect_message(use_testignore("blubb2"))
  tdat3 <- readLines(ti)

  expect_identical(tdat2, tdat3)

  # Cleanup
  unlink(file.path(tpkg, "tests/testthat/_testignore"))
  expect_identical(fs, list.files("."))
  usethis::proj_set(proj)
})

usethis::proj_set(proj)
