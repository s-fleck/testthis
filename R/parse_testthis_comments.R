extract_testthis_params <- function(infile){
  dl <- extract_testthis_comments(infile)

  tokenz <- lapply(dl, testthis_tokenizer)

}


extract_testthis_comments <- function(infile){

  dat <- readLines(infile)
  dat <- dat[detect_testthis_comments(dat)]

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
