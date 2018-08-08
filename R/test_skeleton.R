#' Create a test skeleton file for the currently open .R file
#'
#' If the file currently open in the RStudio editor is called \code{my_function.R},
#' this creates the file \file{/tests/testthat/test_my_function.R} and fills it
#' with a basic test skeleton.
#'
#' @section Side effects:
#'   Creates an R script file in the file system.
#'
#' @param fname Character scalar. Target R script file to open. If empty the file
#'   currently open in the editor will be used.
#' @param open Logical scalar. Should the test file be opened after it is created?
#' @param sep Character scalar. Separator between \file{test} and \file{fname}
#'   when constructing the test file name. Should either be `"_"` or `"-"` for
#'   compatibility with testthat.
#'
#' @return `NULL` (invisibly)
#' @seealso [usethis::use_test()]
#'
#' @export
#'
test_skeleton <- function(
  fname = NULL,
  open = TRUE,
  sep = options('testthis.sep')
){
  require_rstudio()
  ensure_testthat()

  if(is.null(fname)){
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

  invisible()
}
