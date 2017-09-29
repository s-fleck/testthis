#' Tell testthis that a function does not need to be tested
#'
#' Tell testthis that a function does not need to be tested
#' (for [get_test_coverage()])
#'
#' @param x a character vector of ignores (function names).
#'
#' @return
#' @export
#' @family infrastructure
#'
#' @examples
#'
#' \dontrun{
#' use_testignore("helperfunction")
#' }
#'
use_testignore <- function(x, base_path = "."){
  testignore <- "tests/testthat/_testignore"
  testignore <- file.path(devtools::as.package(base_path)$path, testignore)

  cat(sprintf("Adding '%s' to '%s'", x, testignore))
  if(file.exists(testignore)){
    existing_ignores <- readLines(testignore)
    x <- union(existing_ignores, x)
  }

  writeLines(x, testignore)
}
