#' Test this file
#'
#' If the current file is called "function.R", test_this calls
#' testthat::test_file on "tests/testhat/test_function.R".
#'
#' @export
#' @import rstudioapi testthat
#' @rdname test_this.R
test_this <- function(){
  fname <- paste0('test_', basename(rstudioapi::getActiveDocumentContext()$path))

  if(identical(fname, 'test_')){
    fname <-  paste0('test_', basename(rstudioapi::getSourceEditorContext()$path))
  }
  tf    <- file.path(testthat::test_path(), fname)

  if(file.exists(tf)){
    message('Running tests in ',tf)
    testthat::test_file(tf)
  } else {
    message(tf, ' does not exist.')
  }
}


#' Load complete package and test this file
#'
#' If the current file is called "function.R", test_this calls
#' testthat::test_file on "tests/testhat/test_function.R".
#'
#' @export
#' @rdname test_this.R
lest_this <- function(){
  devtools::load_all()
  test_this()
}
