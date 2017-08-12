#' Create \code{testdata} folder.
#'
#' A folder to put binary data in that are required (only) by your unit tests
#'
#' @param ... Objects to save.
#' @template pkg
#'
#' @export
#' @family infrastructure
#' @examples
#' \dontrun{
#'   use_testdata(letters, LETTERS)
#' }
use_testdata <- function(
  ...,
  pkg = "."
){
  base_path <- as.package(pkg)$path
  usethis::use_directory(file.path("tests", "testthat", "testdata"), ignore = FALSE, base_path = pkg)
  message(
    "* You can save data files for tests via `save_test()`\n",
    "* Scripts that produce test data should go in testdata-raw"
  )

  invisible(TRUE)
}




#' Create \code{testdata-raw} folder.
#'
#' A folder to put scripts in that produce the files in \file{testdata}
#'
#' @param pkg Package in which to create \file{testdata-raw}. Defaults to
#'   package in current working directory.
#'
#' @export
#' @family infrastructure
use_testdata_raw <- function(pkg = ".") {
  base_path <- as.package(pkg)$path
  usethis::use_directory(file.path("tests", "testthat", "testdata-raw", ignore = FALSE, base_path = pkg))
  invisible(TRUE)
}




testdata_exists <- function(pkg = '.'){
  dir.exists(file.path(
    devtools::as.package(pkg)$path, "tests", "testthat", "testdata"
  ))
}
