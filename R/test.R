test <- function(pkg = '.', skip = TRUE, ...){
  if(skip){
    fltr <- list_test_files(
      pkg,
      full_names = FALSE,
      skip = TRUE
    )
    fltr <- gsub("^test-?", "", fltr)
    fltr <- gsub("\\.[rR]", "", fltr)

    devtools::test(filter = fltr, ...)
  } else {
    devtools::test(pkg = pkg, ...)
  }
}
