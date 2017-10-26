is_testfile <- function(x){
  is.scalar(x) &&  is_testfiles(x)
}




is_testfiles <- function(x){
  grepl("^test[_\\-]", basename(x))
}




#' Check if a file is /R/
#'
#' @param x a file path
#'
#' @return logical
#' @noRd
#'
is_in_rdir <- function(x) {
  pat <- file.path('/R', basename(x))
  grepl(pat, x, fixed = TRUE)
}




get_current_file <- function(){
  require_rstudio()
  res <- rstudioapi::getActiveDocumentContext()$path

  if (identical(res, '')){
    res <-  rstudioapi::getSourceEditorContext()$path
  }

  res
}




get_pkg_testfile_names_from_tags <- function(){
  infiles <- list_rdir_files()

  res <- lapply(infiles, get_taglist) %>%
    lapply(get_tag, 'testfile') %>%
    stats::setNames(infiles) %>%
    unlist()

  data.frame(
    rfile = names(res),
    tfile = paste0(as.character(res), '.R'),
    stringsAsFactors = FALSE
  )
}




get_testfile_name_from_tag <- function(infile){
  assert_that(is.scalar(infile))

  res <- infile %>%
    get_taglist() %>%
    get_tag('testfile')

  if(is.null(res)) return(NULL)

  data.frame(
    rfile = infile,
    tfile = paste0(res, '.R'),
    stringsAsFactors = FALSE
  )
}
