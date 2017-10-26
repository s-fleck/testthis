#' Tell testthis that a function does not need to be tested
#'
#' Tell testthis that a function does not need to be tested
#' (for [get_test_coverage()])
#'
#' @param x a character vector of ignores (function names).
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
  x
){
  # Preconditions
    assert_that(is.character(x))

  # Process arguments
    testignore <- "tests/testthat/_testignore"
    testignore <- file.path(usethis::proj_get(), testignore)

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
