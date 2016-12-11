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
test_skeleton <- function(open = TRUE){
  fname <- get_testfile_name()

  if(file.exists(fname)){
    message(sprintf('* File alread exists: %s', fname))
  } else {
    message(sprintf('* Creating `%s`', fname))
    title_name <- tools::file_path_sans_ext(bn)

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
