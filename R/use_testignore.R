#' Tell testthis that a function does not need to be tested
#'
#' Tell testthis that a function does not need to be tested
#' (for [get_test_coverage()])
#'
#' @param x a character vector of ignores (function names).
#' @template base_path
#'
#' @return `TRUE` (invisibly).
#' @export
#' @family infrastructure
#'
#' @examples
#'
#' \dontrun{
#' use_testignore("helperfunction")
#' }
#'
use_testignore <- function(
  x,
  base_path = "."
){
  # Preconditions
    assert_that(is.character(x))
    assert_that(is.character(base_path) && is.scalar(base_path))

  # Process arguments
    testignore <- "tests/testthat/_testignore"
    testignore <- file.path(devtools::as.package(base_path)$path, testignore)

  # Logic
    if(file.exists(testignore)){
      existing_ignores <- readLines(testignore)
      x <- union(existing_ignores, x)
    }

    x_frmt <- paste(sprintf("'%s'", x), collapse = ", ")
    message(sprintf("Adding %s to '%s'", x_frmt, testignore))
    writeLines(x, testignore)

  invisible()
}
