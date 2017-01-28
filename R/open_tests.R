#' Open associated test_file
#'
#' If the currently open file in the Rstudio editor is called \code{myfun.R} this
#' opens /code{tests/testthat/test_myfun.R} in a new tab.
#'
#' @export
open_tests <- function(){
  fname <- get_testfile_name()

  if(file.exists(fname)){
    rstudioapi::navigateToFile(fname)
  } else {
    test_skeleton(fname, open = TRUE)
  }
}


#' Create a test skeleton file  for the currently open .R file
#'
#' If the file currently open in the Rstudio editor is called \code{my_function.R},
#' this creates the file \code{/tests/testthat/test_my_function.R} and fills it
#' with a basic test skeleton.
#'
#' @param open Should the test file be opend after it is created?
#'
#' @rdname create_tests
#' @export
#' @import rstudioapi testthat
test_skeleton <- function(fname, open = TRUE){
  if(missing(fname)){
    fname <- get_testfile_name()
  }


  if(file.exists(fname)){
    message(sprintf('* File alread exists: %s', fname))
  } else {
    message(sprintf('* Creating `%s`', fname))
    title_name <- stringi::stri_sub(tools::file_path_sans_ext(basename(fname)), 6)

    lines <- paste0(
      sprintf('context("%s")', title_name),
      "\n\n\n",
      sprintf('test_that("%s works as expected", {\n\n\n})', title_name)
    )

    writeLines(lines, fname)
  }

  if(open){
    rstudioapi::navigateToFile(fname)
  }
}
