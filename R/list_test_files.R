list_test_files <- function(pkg, full_names = TRUE, skip = FALSE){
  pkg     <- devtools::as.package(pkg)
  tpath   <- file.path(pkg$path, 'tests', 'testthat')
  res     <- list.files(
    tpath,
    full.names = TRUE
  )

  res <- res[grepl('.*\\.R$', res, ignore.case = TRUE)]

  if(skip){
    res <- skip_test_files(res)
  }

  if(identical(full_names, FALSE)){
    res <- basename(res)
  }

  return(res)
}


skip_test_files <- function(ttfiles){
  skip <- vector('logical', length(ttfiles))
  for(i in seq_along(ttfiles)){
    x <- get_taglist(ttfiles[[i]])
    skip[[i]] <- !is.null(get_tag(x, 'skip'))
  }
  return(ttfiles[!skip])
}
