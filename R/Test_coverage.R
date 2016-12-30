test_coverage <- function(dat){
  class(dat) <- c('Test_coverage', 'data.frame')
  return(dat)
}


#' @export
print.Test_coverage <- function(dat){

  tp    <- sum(dat$tested)/ nrow(dat) * 100
  pname <- attr(dat, 'package')
  if(is.null(pname)) pname <- ''

  msg   <- sprintf('Package %s, Test Coverage: %.1f%%\n', pname, tp)

  dd  <- dat
  dd$tested <- ifelse(dat$tested, '+', '')

  dexp <- dd[dd$exp == TRUE, ]
  dexp <- dexp[order(dexp$fun), c('tested', 'fun')]
  names(dexp) <- c('', '')

  dint <- dd[dd$exp == FALSE, ]
  dint <- dint[order(dint$fun), c('tested', 'fun')]
  names(dint) <- c('', '')

  cat(msg, '\n')

  hline <- paste(rep('.', 20), collapse = '')

  if(nrow(dexp) > 0){
    cat(' exported functions', hline)
    print.data.frame(dexp, row.names = FALSE, right = FALSE)
  }

  if(nrow(dint) > 0){
    if(nrow(dexp) > 0) cat('\n')
    cat(' internal functions', hline)
    print.data.frame(dint, row.names = FALSE, right = FALSE)
  }

  # cat(fline, '\n')
  invisible(dat)
}


#' @export
get_test_coverage <- function(pkg = '.', ...){
  all  <- get_package_functions(pkg = pkg)
  tst  <- get_tested_functions(pkg = pkg, ...)
  exp  <- get_exported_functions(pkg = pkg)

  res <- data.frame(
    fun    = all,
    exp    = all %in% exp,
    tested = all %in% tst
  )

  res <- res[order(res$fun), ]

  attr(res, 'package') <- devtools::as.package(pkg)$package
  test_coverage(res)
}
