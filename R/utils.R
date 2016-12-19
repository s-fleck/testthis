get_testfile_name <- function(){

  fname <- rstudioapi::getActiveDocumentContext()$path

  if(identical(fname, '')){
    fname <-  rstudioapi::getSourceEditorContext()$path
  }

  scriptfile <- readLines(fname)
  opts       <- parse_options(fname)

  if(is.null(opts$testfile)){
    bn <- basename(fname)

    if(grepl('^test_', bn)){
      res <- fname
    } else {
      res <- file.path(testthat::test_path(), paste0('test_', bn))
    }
  } else {
    bn  <- paste0(opts$testfile, '.R')
    res <- file.path(testthat::test_path(), bn)
  }

}


parse_options <- function(fname){
  dat <- readLines(fname)

  options    <- dat[grep('^#! ', dat)] %>%
    stringi::stri_sub(4) %>%
    stringi::stri_split(fixed = '=') %>%
    lapply(trimws)

  res <- list()
  for(el in options){
    res[[el[[1]]]] = el[[2]]
  }

  return(res)
}
