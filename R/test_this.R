#' Test this file
#'
#' Runs testthat tests in a single .R file. If the file currently open in the
#' RStudio editor is called `my_function.R`, `test_this()` calls
#' `testthat::test_file()` on \file{tests/testthat/test_my_function.R}. If
#' the filename of the currently open file with starts with `test_` it will
#' call `testthat::test_file()` on the current file.
#'
#' This is useful in cases where you don't want to run all tests in a package
#' via `devtools::test()` (CTRL+SHIFT+T).
#'
#' @param ... passed on to [testthat::test_file()]
#'
#' @export
#' @return `NULL` (invisibly)
#'
#' @rdname test_this.R
test_this <- function(...){
  fname <- get_testfile_name()

  if (requireNamespace("rstudioapi", quietly = TRUE)){
    rstudioapi::documentSaveAll()
  }

  devtools::load_all(usethis::proj_get())

  if(file.exists(fname)){
    message("Running tests in ", fname)
    testthat::test_file(fname, ...)
  } else {
    msg_testfile_does_not_exist(fname)
  }

  invisible()
}




#' \code{lest_this()} Deprecated. Please us `test_this()` instead.
#' @export
#' @rdname test_this.R
lest_this <- function(...){
  .Deprecated(
    msg = paste(
      "'test_this()' now reloads package by default. 'lest_this()' is no",
      "longer required and will be dropped in future versions."
    ))
  test_this(...)
}
