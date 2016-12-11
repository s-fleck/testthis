#' Test this file
#'
#' If the file currently open in the Rstudio editor is called \code{my_function.R},
#' \code{test_this()} calls \code{testthat::test_file()} on "tests/testhat/test_my_function.R".
#' This is usefull in cases where you don't want to run all tests in a package
#' via \code{devtools::test()} (CTRL+SHIFT+T)
#'
#' @export
#' @import rstudioapi testthat
#' @rdname test_this.R
test_this <- function(){
  fname <- get_testfile_name()

  if(file.exists(fname)){
    message('Running tests in ', fname)
    testthat::test_file(fname)
  } else {
    message(fname, ' does not exist.')
  }
}


#' \code{lest_this()} does the same, but calls \code{devtools::load_all()} first.
#' @export
#' @rdname test_this.R
lest_this <- function(){
  devtools::load_all()
  test_this()
}
