#' Open associated test file
#'
#' If the currently open file in the RStudio editor is called \file{myfun.R} this
#' opens \file{tests/testthat/test_myfun.R} in a new tab. This function can also
#' be used to jump back and forth between a script and the associated test
#' file. You can modify this behaviour by putting the comment
#' `#* @testfile anotherfile` anywhere in \file{myfun.R}.
#'
#' @export
open_testfile <- function(){
  require_rstudio()

  cfile <- get_current_file()

  if(!is_testfile(cfile) || is_in_rdir(cfile)){
    fname <- get_testfile_name()

    if(file.exists(fname)){
      rstudioapi::navigateToFile(fname)
    } else {
      msg_testfile_does_not_exist(fname)
    }


  } else {
    open_associated_rfile(cfile)
  }
}




open_associated_rfile <- function(x){
  tag_paths <- get_pkg_testfile_names_from_tags()
  bn <- basename(x)

  tp <- tag_paths[grep(bn, tag_paths$tfile, fixed = TRUE), ]

  if (identical(nrow(tp), 0L)){
    rfile <- bn %>%
      gsub('^test[_\\-]', '', .) %>%
      file.path(devtools::as.package('.')$path, 'R', .)
  } else {
    if(nrow(tp) > 1L) {
      warning('More than one @testfile tag present. Using first.')
    }

    rfile <- tp$rfile[[1]]
  }

  assert_that(file.exists(rfile))

  if(file.exists(rfile)){
    rstudioapi::navigateToFile(rfile)
  } else {
    stop(
      sprintf('Associated rfile %s does not found ', rfile),
      'This is likely a bug in the testthis package. Please contact maintainer.'
    )
  }
}
