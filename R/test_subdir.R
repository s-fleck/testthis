#' Execute all test_that tests in a subdir.
#'
#' This is a simple wrapper for [devtools::test()], but rather than running
#' the tests in `inst/tests/` or `tests/testthat`, it runs the tests in a
#' specified subdirectory of that folder.
#'
#' Three preset test subdirs are defined at the moment:
#'
#' \tabular{ll}{
#'   `test_integration()` \tab Integration tests, also called component tests.
#'     Put tests here that test if several functions / parts of your program
#'     work together as expected \cr
#'   `test_acceptance()` \tab Acceptance tests. This is the highest levels of
#'     tests. Put tests here that verifies if your package fulfills the
#'     goals/requirements you set out to achieve with your package were met. \cr
#'   `test_manual()` \tab Manual tests. Put tests here that produce output
#'     that has to be manually verified, such as: console output, pdf files,
#'     plots. It is recommended you collect the output files of such tests in
#'     'tests/testthat/test_out'. \cr
#' }
#'
#' The above functions assume the respective test files are in
#' `testthat/integration_tests`, `testthat/acceptance_tests` or
#' `testthat/manual_tests`
#'
#' @inheritParams devtools::test
#' @template base_path
#' @param subdir subdir of `inst/tests/` or `tests/testthat` that contains the
#'   tests
#' @param ... passed on to `devtools::test()`
#'
#' @return `NULL` (invisibly)
#'
#' @export
test_subdir <- function(subdir, base_path = '.', ...){

  find_test_dir_mock <- function (path)
  {
    testthat <- file.path(path, "tests", "testthat", subdir)
    if (dir.exists(testthat))
      return(testthat)
    inst <- file.path(path, "inst", "tests", subdir)
    if (dir.exists(inst))
      return(inst)
    stop(
      sprintf("%s not found in any of the test dirs of %s", subdir, path),
      call. = FALSE
    )
  }


  testthat::with_mock(
    `devtools:::find_test_dir` = find_test_dir_mock,
    devtools::test(pkg = base_path, ...)
  )

  invisible()
}




#' @rdname test_subdir
#' @export
test_integration <- function(base_path = '.', ...){
  test_subdir(
    subdir = options('testthis.integration_tests_path'),
    base_path = base_path,
    ...)
}




#' @rdname test_subdir
#' @export
test_acceptance <- function(base_path = '.', ...){
  test_subdir(
    subdir = options('testthis.acceptance_tests_path'),
    base_path = base_path,
    ...)
}




#' @rdname test_subdir
#' @export
test_manual <- function(base_path = '.', ...){
  test_subdir(
    subdir = options('testthis.manual_tests_path'),
    base_path = base_path,
    ...)
}
