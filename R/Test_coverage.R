test_coverage <- function(dat){
  class(dat) <- c('Test_coverage', 'data.frame')
  return(dat)
}


print.Test_coverage <- function(dat){

  cat('Package', attr(dat, 'package'), '\n\n')

  print.data.frame(dat, row.names = FALSE, right = FALSE)

  tp <- sum(dat$tested)/ nrow(dat)

  cat(sprintf('\nTest Coverage: %.1f%%', tp))
}

get_test_coverage <- function(pkg = '.'){
  x <- get_tested_functions(pkg = pkg)
  y <- get_package_functions(pkg = pkg)

  res <- data.frame(
    fun    = y,
    tested = y %in% x
  )

  attr(res, 'package') <- devtools::as.package(pkg)$package

  test_coverage(res)
}
