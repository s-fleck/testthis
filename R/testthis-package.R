#' Testthis-package
#'
#' RStudio addins for several common testing-related tasks during package
#' development, such as switching between a source file and an associated test
#' file, or running unit tests in a single test file (hotkeyable in RStudio!).
#' testthis also provides utility function to manage tests in subdirectories of
#' the test/testthis directory.
#'
#' For details please refer to the
#' [README](https://s-fleck.github.io/testthis/index.html)
#'
#' @name testthis
#' @import assertthat
#' @importFrom magrittr %>%
#' @importFrom stats setNames
#'
#' @section Setting the project path:
#'
#' Testthis uses [usethis::proj_get()] to detect project root of the current
#' working directory. You can override the project root with
#' [usethis::proj_set()].
#'
#' @section Package options:
#'
#' Package options can be set with [`options()`]. You can add `options()` to
#' your \file{.Rprofile} to make them permanent across sessions.
#'
#' \describe{
#'
#' \item{`testthis.sep`}{Default separator to use when creating test files with
#'   `test_skeleton()`. Defaults to `_`, must be either `_` or `-`; i.e whether
#'   you want your files to be named `test_foofunction.R` or `test-foofunction.R`}
#'
#' \item{`testthis.integration_tests_path`, `testthis.acceptance_tests_path`,
#' `testthis.manual_tests_path`}{Default paths used by the functions
#'  `testthis::use_integration_tests()`, `testthis::test_integration()`, etc...}
#'}
#'
#'
#' @section Testthis Tags:
#'
#' test_this tags are special comments that modify the behaviour of the
#' functions supplied by this package. They are of the form `#* @tag <value>`.
#' Please not that only some test_this tags really require a `<value>`.
#'
#' **Valid tags for script files in the /R/ dir (`pkg/R/*.R`)**
#'
#' * `@testfile <filename>`: manually specify associated test file. Should
#' usually start with `test_`. This is used by `test_this()`, `lest_this()` and
#' `open_testfile()`.
#'
#'
#' **Valid tags or test files (`pkg/tests/testthat/test_*.R`)**
#'
#' * `@testing <functionname>`:  mark `functionname` as tested.
#' Should usually go next the associated `test_that()` call. This is used by
#' `test_coverage()`.
#'
#'
#' @seealso [usethis::edit_r_profile()]
#' @docType package
"_PACKAGE"








if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))



.onLoad <- function(...) {
  op <- options()

  op.testthis <- list(
    testthis.sep = '_',
    testthis.integration_tests_path = "integration_tests",
    testthis.acceptance_tests_path = "acceptance_tests",
    testthis.manual_tests_path = "manual_tests"
  )

  toset <- !(names(op.testthis) %in% names(op))
  if(any(toset)) options(op.testthis[toset])

  if (requireNamespace("crayon", quietly = TRUE) && crayon::has_color()){

    style_error   <- crayon::make_style("#BB3333", colors = 256)
    style_fatal   <- function(...) style_error(crayon::bold(...))
    style_warning <- crayon::make_style("#EEBB50", colors = 256)
    style_subtle  <- crayon::make_style(grDevices::grey(0.5), grey = TRUE)
    style_accent  <- crayon::make_style("#ca2c92", colors = 256)
    col_nchar     <- crayon::col_nchar

  } else {
    style_fatal   <- function(...) paste(...)
    style_error   <- style_fatal
    style_warning <- style_fatal
    style_subtle  <- style_fatal
    style_accent  <- style_fatal
    col_nchar     <- function(...) nchar(...)
  }

  assign("style_fatal", style_fatal, envir = parent.env(environment()))
  assign("style_error", style_error, envir = parent.env(environment()))
  assign("style_warning", style_warning, envir = parent.env(environment()))
  assign("style_subtle", style_subtle, envir = parent.env(environment()))
  assign("style_accent", style_accent, envir = parent.env(environment()))
  assign("col_nchar", col_nchar, envir = parent.env(environment()))

  invisible()
}
