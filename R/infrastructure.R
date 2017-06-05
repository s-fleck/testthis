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
  pkg <- as.package(pkg)

  use_directory(file.path("tests", "testthat", "testdata"), ignore = FALSE, pkg = pkg)

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
  pkg <- as.package(pkg)

  use_directory(file.path("tests", "testthat", "testdata-raw", ignore = FALSE, pkg = pkg))

  invisible(TRUE)
}



# cited from devtools
use_directory <- function(
  path,
  buildignore = FALSE,
  pkg = "."
){
  pkg <- as.package(pkg)
  pkg_path <- file.path(pkg$path, path)

  if (file.exists(pkg_path)) {
    if (!dir.exists(pkg_path)) {
      stop("`", path, "` exists but is not a directory.", call. = FALSE)
    } else {
      message("* Directory `", path, "` already exists.")
    }
  } else {
    message("* Creating `", path, "`.")
    dir.create(pkg_path, showWarnings = FALSE, recursive = TRUE)
  }

  if (buildignore) {
    message("* Adding `", path, "` to `.Rbuildignore`.\n")
    devtools::use_build_ignore(path, pkg = pkg)
  }

  invisible(TRUE)
}



dots <- function(...) {
  eval(substitute(alist(...)))
}



testdata_exists <- function(pkg = '.'){
  dir.exists(file.path(
    devtools::as.package(pkg)$path, "tests", "testthat", "testdata"
  ))
}
