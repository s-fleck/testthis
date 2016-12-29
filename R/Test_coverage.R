test_coverage <- function(dat){
  class(dat) <- c('Test_coverage', 'data.frame')
  return(dat)
}


#' @export
print.Test_coverage <- function(dat){

  tp  <- sum(dat$tested)/ nrow(dat) * 100
  msg <- sprintf('Package %s, Test Coverage: %.1f%%\n', attr(dat, 'package'), tp)

  dd  <- dat
  dd$tested <- ifelse(dat$tested, '+', ' ')
  names(dd) <- c('', '')

  cat(msg)
  print.data.frame(dd, row.names = FALSE, right = FALSE)
  cat('\n')

  invisible(dat)
}


#' @export
get_test_coverage <- function(pkg = '.', ...){
  all  <- get_package_functions(pkg = pkg)
  tst  <- get_tested_functions(pkg = pkg, ...)

  res <- data.frame(
    tested = all %in% tst,
    fun    = all
  )

  attr(res, 'package') <- devtools::as.package(pkg)$package

  test_coverage(res)
}
