test_coverage <- function(dat){
  class(dat) <- c('Test_coverage', 'data.frame')

  assert_that(is_valid(dat))
  return(dat)
}


is_valid.Test_coverage <- function(dat){
  res <- list()

  res$names <- assert_that(identical(
    c('fun', 'exp', 'tested'),
    names(dat))
  )
  res$types <- assert_that(identical(
    unname(unlist(lapply(dat, class))),
    c('character', 'logical', 'logical'))
  )

  all(unlist(res))
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
  all  <- get_all_functions(pkg = pkg)
  tst  <- get_tested_functions(pkg = pkg, ...)
  exp  <- get_exported_functions(pkg = pkg)

  res <- data.frame(
    fun    = all,
    exp    = all %in% exp,
    tested = all %in% tst,
    stringsAsFactors = FALSE
  )

  attr(res, 'package') <- devtools::as.package(pkg)$package
  test_coverage(res)
}
