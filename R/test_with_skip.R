#' Execute all test_that tests in a package, except some.
#'
#' This is a wrapper for [devtools::test()] that skips `test_*.R`
#' files that contain the testthis tag `#' @skip`.
#'
#' See the devtools documentation for further info or
#' `vignette("testthis")` for infos on testthis tags.
#'
#' @inheritParams devtools::test
#'
#' @export
test_with_skip <- function(...){
  fltr <- list_test_files(
    full_names = FALSE,
    skip = TRUE
  )
  fltr <- gsub("^test-?", "", fltr)
  fltr <- gsub("\\.[rR]", "", fltr)
  fltr <- paste(fltr, collapse = '|')
  assert_that(is.scalar(fltr) && is.character(fltr))

  devtools::test(filter = fltr, ...)
}
