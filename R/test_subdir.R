#' Run tests in subdirectories
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
#' @param subdir `character` scalar. subdir of `inst/tests/` or `tests/testthat`
#'   that contains the tests
#' @param ... passed on to `devtools::test()`
#' @seealso [`use_test_subdir()`]
#'
#' @return A [testthat::testthat_results] object (invisibly)
#'
#' @export
test_subdir <- function(subdir, ...){
  if (requireNamespace("rstudioapi", quietly = TRUE)){
    rstudioapi::documentSaveAll()
  }

  devtools::load_all(usethis::proj_get())

  testthat::test_dir(file.path(
    usethis::proj_get(), testthat::test_path(), subdir
  ))

}




#' @rdname test_subdir
#' @export
test_integration <- function(...){
  test_subdir(
    subdir = options("testthis.integration_tests_path"),
    ...)
}




#' @rdname test_subdir
#' @export
test_acceptance <- function(...){
  test_subdir(
    subdir = options("testthis.acceptance_tests_path"),
    ...)
}




#' @rdname test_subdir
#' @export
test_manual <- function(...){
  test_subdir(
    subdir = options("testthis.manual_tests_path"),
    ...)
}




#' `test_all()` Runs the tests in \file{tests/testthat} and all its
#'   subdirectories (except \file{testdata} and \file{testdata-raw}).
#' @rdname test_subdir
#' @export
test_all <- function(
  ...
){
  if (requireNamespace("rstudioapi", quietly = TRUE)){
    rstudioapi::documentSaveAll()
  }

  devtools::load_all(usethis::proj_get())

  pkg_dir <- usethis::proj_get()

  dirs    <- basename(
    list.dirs(
      file.path(pkg_dir, testthat::test_path()),
      recursive = FALSE
    )
  )

  dirs <- dirs[dirs != "testdata"]

  testthat::with_mock(
    devtools::test(...),
    `testthat::find_test_scripts` = find_test_scripts_mock
  )
}





dir_mock <- function(path = ".", pattern = NULL, ...){
  list.files(path  = path, pattern = pattern, recursive = TRUE)
}


find_test_scripts_mock <- function(
  path,
  filter = NULL,
  invert = FALSE,
  ...
){
  files <- dir(
    path, "^test.*\\.[rR]$",
    full.names = TRUE,
    recursive = TRUE
  )

  files <- grep(
    "tests/testthat/test(data/)|(data-raw/)",
    files,
    value = TRUE,
    invert = TRUE
  )

  if(!is.null(filter)){
    stop("filter not supported")
  }

  files
}
