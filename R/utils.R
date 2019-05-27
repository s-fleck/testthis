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
    lapply(get_tag, "testfile") %>%
    stats::setNames(infiles) %>%
    unlist()


  if (is_empty(res)){
    data.frame(
      rfile = character(),
      tfile = character()
    )
  } else {
    data.frame(
      rfile = names(res),
      tfile = ifelse(
        tools::file_ext(res) == "",
        paste0(as.character(res), ".R"),
        as.character(res)
      ),
      stringsAsFactors = FALSE
    )
  }
}




get_testfile_name_from_tag <- function(infile){
  assert_that(is.scalar(infile))

  res <- infile %>%
    get_taglist() %>%
    get_tag('testfile')

  if(is.null(res)) return(NULL)

  data.frame(
    rfile = infile,
    tfile = ifelse(
      tools::file_ext(res) == "",
      paste0(as.character(res), ".R"),
      as.character(res)
    ),
    stringsAsFactors = FALSE
  )
}




ensure_testthat <- function(
  base_path = usethis::proj_get()
){
  if (!dir.exists(file.path(base_path, "tests", "testthat")))
    usethis::use_testthat()
}




path_diff <- function(x, y){
  assert(is.character(x))
  assert(is_scalar_character(y))
  xs <- fs::path_split(x)
  ys <- unlist(fs::path_split(y))

  vapply(
    xs,
    function(.x) do.call(file.path, as.list(setdiff(unlist(.x), ys))),
    character(1)
  )
}

