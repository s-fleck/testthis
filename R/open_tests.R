#' Open associated test_file
#'
#' If the currently open file in the Rstudio editor is called \code{myfun.R} this
#' opens \file{tests/testthat/test_myfun.R} in a new tab.
#'
#' @export
open_tests <- function(){
  if(!requireNamespace("rstudioapi")){
    stop('This function is designed to be used from within Rstudio')
  }

  fname <- get_testfile_name()

  if(file.exists(fname)){
    rstudioapi::navigateToFile(fname)
  } else {
    message('No associated testfile exists. You can create one with test_skeleton()')
  }
}


#' Create a test skeleton file  for the currently open .R file
#'
#' If the file currently open in the Rstudio editor is called \code{my_function.R},
#' this creates the file \file{/tests/testthat/test_my_function.R} and fills it
#' with a basic test skeleton.
#'
#' @section Side effects:
#'   Creates a file.
#'
#' @param fname optional: Targt R script file to open. If empty the file
#'   currently open in the editor will be used.
#' @param open Should the test file be opend after it is created?
#'
#' @export
#'
test_skeleton <- function(
  fname,
  open = TRUE,
  sep = options('testthis.sep')
){
  if(!requireNamespace("rstudioapi")){
    stop('This function is designed to be used from within Rstudio')
  }

  if(missing(fname)){
    fname <- get_testfile_name(sep = sep)
  }

  if(file.exists(fname)){
    message(sprintf('* File alread exists: %s', fname))
  } else {
    message(sprintf('* Creating `%s`', fname))
    title_name <- stringi::stri_sub(
      tools::file_path_sans_ext(basename(fname)), 6
    )

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
