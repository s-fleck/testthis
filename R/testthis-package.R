#' Testthis-package
#'
#' @name testthis
#' @import assertthat
#' @importFrom magrittr %>%
#'
#' @section Introduction:
#'
#'   Testthis provides several tools to make unit testing in R slightly more
#'   comfortable. It is designed to be used in conjunction with the packages
#'   \pkg{testthat}, \pkg{usethis} packages and \pkg{devtools}.
#'
#'   For more info please refer to the package vignette via `vignette("testthis")`
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
