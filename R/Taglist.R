#' Testthis Taglist
#'
#' @param dat
#'
#' Possible tags:
#'    testfile *path_to_file* ... Put this in a file to manually set the name of an associated
#'    test file (relevant for \code{test_this}, \code{open_tests}, etc...)
#'
#'    testing *function_name* ... put this in a testfile to mark the current file includes
#'    tests for a functiuon
#'
#' @return
#' @export
#'
#' @examples
taglist <- function(dat){
  class(dat) <- c('Taglist', 'list')
  return(dat)
}


get_tag <- function(dat, tag){
  assert_that('Taglist' %in% class(dat))

  select_ats <-  function(x) x[[1]] == paste0('@', tag)
  dd <- dat[unlist(lapply(dat, select_ats))]
  extract_second <- function(x) x[[2]]
  unlist(lapply(dd, extract_second))
}


get_taglist <- function(infile){
  dl <- extract_testthis_comments(infile)
  tokenz <- lapply(dl, testthis_tokenizer)
  taglist(tokenz)
}


extract_testthis_comments <- function(infile){
  dat <- readLines(infile)
  dat[detect_testthis_comments(dat)]
}


detect_testthis_comments <- function(x){
  grepl('^\\s*#\\*\\s*@.*\\s\\S*.*', x, ignore.case = TRUE)
}


#' testthis tokenizer
#'
#' @param x
#'
#' @import stringi
#' @return
#'
#' @examples
testthis_tokenizer <- function(x){
  assert_that(is.scalar(x))
  assert_that(detect_testthis_comments(x))

  y <- stringi::stri_extract_first(x, regex = '@\\S*\\s*\\S*')
  y <- stringi::stri_split(y, fixed = ' ')

  unlist(y)
}





