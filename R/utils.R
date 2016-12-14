get_testfile_name <- function(){

  fname <- rstudioapi::getActiveDocumentContext()$path

  if(identical(fname, '')){
    fname <-  rstudioapi::getSourceEditorContext()$path
  }

  bn <- basename(fname)

  if(grepl('^test_', bn)){
    res <- fname
  } else {
    res <- file.path(testthat::test_path(), paste0('test_', bn))
  }

}
