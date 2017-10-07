#' Use test subdirectories
#'
#' This is a simple wrapper for [devtools::test()], but rather than running
#' the tests in \file{inst/tests/} or \file{tests/testthat}, it runs the tests
#' in a subdirectory of that folder. For creating such subdirectories,
#' please also see [`use_test_subdir()`].
#'
#'
#' @section Test subdirectory presets:
#'
#' Three preset test subdirs are defined at the moment:
#'
#' \describe{
#'   \item{`test_integration()`}{Integration tests, also called component tests.
#'     Put tests here that test if several functions / parts of your program
#'     work together as expected. You can create the relevant subdir
#'     \file{testthat/integration_tests/} with `use_integration_tests()`.
#'
#'     }
#'   \item{`test_acceptance()`}{Acceptance tests. This is the highest levels of
#'     tests. Put tests here that verifies if your package fulfills the
#'     goals/requirements you set out to achieve with your package were met.
#'     You can create the relevant subdir \file{testthat/acceptance_tests/}
#'     with `use_acceptance_tests()`.
#'     }
#'   \item{`test_manual()`}{Manual tests. Put tests here that produce output
#'     that has to be manually verified, such as: console output, pdf files,
#'     plots. It is recommended you collect the output files of such tests in
#'     \file{tests/testthat/testout}. You can create the relevant subdir
#'     with \file{testthat/manual_tests/} with `use_manual_tests()`.}
#' }
#'
#' You can modify the default paths for manual, acceptance and integration tests
#' by setting the respective `options()`, but it is recommended to create your
#' own test subdirs instead.
#'
#' @inheritParams devtools::test
#' @template base_path
#' @param subdir subdir of `inst/tests/` or `tests/testthat` that contains the
#'   tests
#' @param ... passed on to `devtools::test()`
#' @seealso [`use_test_subdir()`]
#'
#' @return A [testthat_results] object (invisibly)
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


  invisible(testthat::with_mock(
    `devtools:::find_test_dir` = find_test_dir_mock,
    devtools::test(pkg = base_path, ...)
  ))
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
