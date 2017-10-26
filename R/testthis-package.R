#' Testthis-package
#'
#' RStudio addins for several common testing-related tasks during package
#' development, such as switching between a source file and an associated test
#' file, or running unit tests in a single test file (hotkeyable!).
#' testthis also provides utility function to manage tests in subdirectories of
#' the test/testthis directory.
#'
#' For details please refer to `vignette("testthis")`
#'
#' @name testthis
#' @import assertthat
#' @importFrom magrittr %>%
#'
#' @section Package options:
#'
#' \describe{
#' \item{`testthis.sep`}{Default separator to use when creating test files with
#'   `test_skeleton()`. Defaults to `_`, must be either `_` or `-`; i.e whether
#'   you want your files to be named `test_foofunction.R` or `test-foofunction.R`}
#' }
#'
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

  invisible()
}
