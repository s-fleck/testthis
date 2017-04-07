#' Execute all test_that tests in a subdir.
#'
#' This is a simple wrapper for [devtools::test()], but rather than running
#' the tests in `inst/tests/` or `tests/testthat`, it runs the tests in a
#' specified subdirectory of that folder.
#'
#' `test_integration()` and `test_acceptance()` are exemplrary presets for
#' integration and acceptance tests in subdirs names `integration` or
#' `acceptance`.
#'
#' @inheritParams devtools::test
#' @param subdir subdir of `inst/tests/` or `tests/testthat` that containts the
#'   tests
#' @param ... passed on to `devtools::test()`
#'
#' @md
#' @export
test_subdir <- function(subdir, pkg = '.', ...){

  find_test_dir_mock <- function (path)
  {
    testthat <- file.path(path, "tests", "testthat", subdir)
    if (dir.exists(testthat))
      return(testthat)
    inst <- file.path(path, "inst", "tests", subdir)
    if (dir.exists(inst))
      return(inst)
    stop("No testthat directories found in ", path, call. = FALSE)
  }


  testthat::with_mock(
    `devtools:::find_test_dir` = find_test_dir_mock,
    devtools::test(pkg = pkg, ...)
  )
}




#' @rdname test_subdir
#' @export
test_integration <- function(pkg = '.', ...){
  test_subdir(subdir = 'integration', pkg = pkg, ...)
}




#' @rdname test_subdir
#' @export
test_acceptance <- function(pkg = '.', ...){
  test_subdir(subdir = 'acceptance', pkg = pkg, ...)
}

