#* @testfile test_blah_blubb

#' Testthis Taglist
#'
#' This is a simple s3 constructor that does not yet perform any checks. It's
#' only used by get_taglist
#'
#' Possible tags:
#'
#' testfile *path_to_file* ... Put this in a file to manually set
#' the name of an associated test file (relevant for `test_this()`,
#' `open_testfile()`, etc...)
#'
#' testing *function_name* ... put this in a testfile to mark the current file
#' includes tests for a function
#'
#' @param dat a list
#' @noRd
#'
taglist <- function(dat){
  class(dat) <- c('Taglist', 'list')
  return(dat)
}




get_rdir_taglist <- function(){
  ttfiles  <- list_rdir_files(full_names = TRUE)
  lapply(ttfiles, get_taglist)
}




get_test_taglist <- function(){
  ttfiles  <- list_test_files(full_names = TRUE)
  taglists <- lapply(ttfiles, get_taglist)
}




get_taglist <- function(infile){
  assert_that(is.character(infile))
  assert_that(is.scalar(infile))
  assert_that(is.readable(infile))

  dl <- extract_testthis_comments(infile)
  tokens <- lapply(dl, testthis_tokenizer)

  res <- list()
  for(el in tokens){
    tag   <- el[[1]]
    assert_that(stringi::stri_detect_regex(tag, '^@\\w*$'))
    tag <- stringi::stri_replace_all_fixed(tag, "@", "")

    if(length(el) > 1){
      value <- el[2:length(el)]
    } else {
      value <- TRUE
    }

    res[[tag]] <- sort(union(res[[tag]], value))
  }

  if(length(res) > 1){
    res <- res[order(names(res))]
  }

  taglist(res)
}




get_tag <- function(dat, tag){
  assert_that('Taglist' %in% class(dat))
  dat[[tag]]
}




# utils -------------------------------------------------------------------

extract_testthis_comments <- function(infile){
  dat <- readLines(infile)
  dat[detect_testthis_comments(dat)]
}




detect_testthis_comments <- function(x){
  grepl('^\\s*#\\*\\s*@.*', x, ignore.case = TRUE)
}




#' testthis tokenizer
#'
#' @param x a scalar character containing testthis tags
#' @noRd
#'
testthis_tokenizer <- function(x){
  assert_that(is.scalar(x))
  assert_that(detect_testthis_comments(x))

  y <- stringi::stri_extract_first(x, regex = '@\\S*\\s*\\S*')
  y <- stringi::stri_split(y, fixed = ' ')

  unlist(y)
}
