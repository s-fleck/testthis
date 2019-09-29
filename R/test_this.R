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
    filter <- paste0("^", gsub("^.*test-([^/]*)[.][rR]$", "\\1", fname), "$")

    message("Running tests using filter: ", filter)
    devtools::test(usethis::proj_get(), filter = filter, ...)
  } else {
    devtools::load_all(usethis::proj_get())

    msg_testfile_does_not_exist(fname)
  }

  invisible()
}
