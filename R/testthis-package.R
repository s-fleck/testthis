#' Testthis-package
#'
#' @name testthis
#' @import assertthat
#' @importFrom magrittr %>%
#'
#' @section Introduction:
#'
#'   Testthis provides several tools to make unit testing in R slightly more
#'   comfortable. It is designed to be used in conjunction with \code{testthat}
#'   package. I modelled this package after my own workflow so many of the
#'   functions it provides expect that you organize your tests in a certain way.
#'
#' @section Rstudio addins (requires Rstudio):
#'
#'   This package contains several simple Rstudio addins. The motivation behind
#'   defining those function as Rstudio addins is that you can easily assign
#'   hotkeys to them in Rstudio (\code{Tools/Modify Keyboard Shortcuts})
#'
#'   Testthis assumes that for each \code{anyfile.R} file in your package`s
#'   \code{R/} folder you have a testfile \code{/test_anyfile.R} in your test
#'   dir (usually \code{/tests/testthat/}). If you want your testfile to be
#'   named differently, you can add the comment \code{#* @testfile
#'   someotherfile} anywhere in \code{anyfile.R}
#'
#'   \describe{ \item{test_this}{Tests the currently open file}
#'   \item{lest_this}{(("Load and test") devtools::load_all and test the
#'   currently open file} \item{open_testfile}{ If the current filename is
#'   \code{currentfile.R}, this this opens
#'   \code{/tests/testthat/test_currentfile.R} in the editor. If the file does
#'   not exists, it is created with a minimal testing infrastructure skeleton.}
#'   }
#'
#' @section Test coverage checklist:
#'
#'   This package provides a checklist-like approach to extracting test-coverage
#'   of a package. If you want automatic analysis of test coverage, you have to
#'   look somewhere else.
#'
#'   To mark \code{anyfunction} as tested, you have to put the comment \code{#*
#'   @testing anyfunction} in any of the files containing your unit tests
#'   (usually you will put the comment right above the actual tests).
#'
#'   Alternatively, you can name the function in \code{desc} argument of your
#'   \code{test_that(...)} call (e.g. \code{test_that("anyfunction works",
#'   ...}). This behaviour can yield false positives if you are not careful, and
#'   can be switched of.
#'
#'   \describe{ \item{get_test_coverage}{Get the test coverage of a package}
#'   \item{get_pkg_functions}{List all functions of a package}
#'   \item{get_pkg_exports}{List all exported functions a package}
#'   \item{get_pkg_exports}{List all tested functions a package} }
#'
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
