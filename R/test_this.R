#' Test this file
#'
#' Runs testthat tests in a single `.R` file. If the file currently open in the
#' RStudio editor is called `my_function.R`, `test_this()` calls
#' `testthat::test_file()` on \file{tests/testthat/test_my_function.R}. If
#' the filename of the currently open file with starts with `test_` it will
#' call `testthat::test_file()` on the current file.
#'
#' This is useful in cases where you don't want to run all tests in a package
#' via `devtools::test()` (CTRL+SHIFT+T in RStudio).
#'
#' @param ... passed on to [testthat::test_file()]
#'
#' @export
#' @return `NULL` (invisibly)
#'
#' @rdname test_this
test_this <- function(...){
  fname <- get_testfile_name()

  if (requireNamespace("rstudioapi", quietly = TRUE)){
    rstudioapi::documentSaveAll()
  }

  if (file.exists(fname)){
    message("Running tests in ", fname)
    devtools::load_all(usethis::proj_get(), helpers = FALSE)
    testthat::test_file(fname, ...)

  } else {
    msg_testfile_does_not_exist(fname)
  }

  invisible()
}
