#' Test this file
#'
#' If the file currently open in the Rstudio editor is called \code{my_function.R},
#' \code{test_this()} calls \code{testthat::test_file()} on "tests/testhat/test_my_function.R".
#' \code{lest_this()} does the same, but calls \code{devtools::load_all()} first.
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

#' @export
#' @rdname test_this.R
lest_this <- function(){
  devtools::load_all()
  test_this()
}
