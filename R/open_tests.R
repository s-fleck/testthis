#' Open associated test_file
#'
#' If the currently open file in the Rstudio editor is called \code{myfun.R} this
#' opens /code{tests/testthat/test_myfun.R} in a new tab.
#'
#' @export
open_tests <- function(){
  rstudioapi::navigateToFile(get_testfile_name())
}
