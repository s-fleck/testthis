get_testfile_name <- function(){

  bn <- basename(rstudioapi::getActiveDocumentContext()$path)

  if(identical(bn, '')){
    bn <-  basename(rstudioapi::getSourceEditorContext()$path)
  }

  fname <- file.path(testthat::test_path(), paste0('test_', bn))
}
