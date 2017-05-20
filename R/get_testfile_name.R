get_testfile_name <- function(sep = options('testthis.sep')){
  sep <- as.character(sep)
  assert_that(identical(sep, '-') || identical(sep, "_"))

  fname <- get_current_file()
  opts  <- get_testfile_name_from_tag(fname)$tfile

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
    bn  <- opts[[1]]
    res <- file.path(testthat::test_path(), bn)
  }

  return(res)
}
