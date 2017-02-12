list_test_files <- function(pkg, full_names = TRUE){
  pkg     <- devtools::as.package(pkg)
  tpath   <- file.path(pkg$path, 'tests', 'testthat')
  res     <- list.files(tpath, full.names = full_names)
}
