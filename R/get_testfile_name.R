get_testfile_name <- function(sep = options('testthis.sep')){
  sep <- as.character(sep)
  assert_that(identical(sep, '-') || identical(sep, "_"))

  if (!requireNamespace("rstudioapi")){
    stop('This function is designed to be used from within Rstudio')
  }

  fname <- rstudioapi::getActiveDocumentContext()$path

  if (identical(fname, '')){
    fname <-  rstudioapi::getSourceEditorContext()$path
  }

  opts       <- get_tag(get_taglist(fname), 'testfile')


  if (identical(length(opts), 0L)){
    bn <- basename(fname)
    test_pattern <- sprintf('^test[_\\-]')
    test_bn_pattern <- paste0(test_pattern, bn)

    res <- testthat::test_path() %>%
      list.files() %>%
      stringi::stri_subset_regex(test_bn_pattern) %>%
      file.path(testthat::test_path(), .)

    if (identical(length(res), 1L)){
      # do nothing
    } else  if (identical(length(res), 0L)){
      res <- file.path(testthat::test_path(), paste0('test', sep, bn))
    } else if (length(res) > 1L){
      warning('Multiple possible test files found. Using ', res[[1]])
      res <- res[[1]]
    }

  } else {
    if (length(opts) > 1) {
      warning('More than one @testfile tag present. Using first.')
    }
    bn  <- paste0(opts[[1]], '.R')
    res <- file.path(testthat::test_path(), bn)
  }

  return(res)
}
