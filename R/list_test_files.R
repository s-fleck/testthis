list_test_files <- function(
  base_path,
  full_names = TRUE,
  skip = FALSE,
  recursive = TRUE
){
  base_path     <- devtools::as.package(base_path)
  tpath   <- file.path(base_path$path, 'tests', 'testthat')
  res     <- list.files(
    tpath,
    full.names = TRUE,
    recursive = recursive
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




list_rdir_files <- function(
  base_path,
  full_names = TRUE,
  skip = FALSE
){
  base_path     <- devtools::as.package(base_path)
  tpath   <- file.path(base_path$path, 'R')
  list.files(
    tpath,
    full.names = full_names
  )
}




skip_test_files <- function(ttfiles){
  skip <- vector('logical', length(ttfiles))
  for(i in seq_along(ttfiles)){
    x <- get_taglist(ttfiles[[i]])
    skip[[i]] <- !is.null(get_tag(x, 'skip'))
  }
  return(ttfiles[!skip])
}
