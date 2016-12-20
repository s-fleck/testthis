get_testfile_name <- function(){

  fname <- rstudioapi::getActiveDocumentContext()$path

  if(identical(fname, '')){
    fname <-  rstudioapi::getSourceEditorContext()$path
  }

  scriptfile <- readLines(fname)
  opts       <- parse_options(fname)

  if(identical(length(opts), 0L)){
    bn <- basename(fname)

    if(grepl('^test_', bn)){
      res <- fname
    } else {
      res <- file.path(testthat::test_path(), paste0('test_', bn))
    }
  } else {
    bn  <- paste0(opts, '.R')
    res <- file.path(testthat::test_path(), bn)
  }

}


parse_options <- function(fname){
  dat <- readLines(fname)

  res <- dat[grep('^# testthis ', dat)] %>%
    stringi::stri_sub(12) %>%
    trimws()

  return(res)
}


