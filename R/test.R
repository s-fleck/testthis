#' Execute all \pkg{test_that} tests in a package.
#'
#' This is a wrapper for \code{\link[devtools]{test}} that skips \code{test_*.R}
#' files that contain the testthis tag "\code{#' @skip}".
#'
#' See the devtools documentation for further info or
#' \code{vignette("testthis")} for infos on testthis tags.
#'
#' @inheritParams devtools::test
#'
#' @export
test_with_skip <- function(pkg = '.', ...){
  fltr <- list_test_files(
    pkg,
    full_names = FALSE,
    skip = TRUE
  )
  fltr <- gsub("^test-?", "", fltr)
  fltr <- gsub("\\.[rR]", "", fltr)

  devtools::test(filter = fltr, ...)
}
