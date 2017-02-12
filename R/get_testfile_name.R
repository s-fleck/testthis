#* @testfile test_get_testfile_name

get_testfile_name <- function(){

  if(!requireNamespace("rstudioapi")){
    stop('This functions are designed to be used with Rstudio')
  }

  fname <- rstudioapi::getActiveDocumentContext()$path

  if(identical(fname, '')){
    fname <-  rstudioapi::getSourceEditorContext()$path
  }

  scriptfile <- readLines(fname)
  opts       <- get_tag(get_taglist(fname), 'testfile')

  if(identical(length(opts), 0L)){
    bn <- basename(fname)

    if(grepl('^test_', bn)){
      res <- fname
    } else {
      res <- file.path(testthat::test_path(), paste0('test_', bn))
    }
  } else {
    if(length(opts) > 1) {
      warning('More than one @testfile tag present. Using first.')
    }
    bn  <- paste0(opts[[1]], '.R')
    res <- file.path(testthat::test_path(), bn)
  }
}



