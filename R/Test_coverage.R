test_coverage <- function(dat){
  class(dat) <- c('Test_coverage', 'data.frame')

  assert_that(is_valid(dat))
  return(dat)
}




is_valid.Test_coverage <- function(dat){
  res <- list()

  res$names <- assert_that(identical(
    c('fun', 'exp', 's3', 'tested'),
    names(dat))
  )
  res$types <- assert_that(identical(
    unname(unlist(lapply(dat, class))),
    c('character', 'logical', 'logical', 'logical'))
  )

  all(unlist(res))
}




#' @export
print.Test_coverage <- function(x, ...){

  # Heading
    pname <- attr(x, 'package')
    if(is.null(pname)) pname <- ''
    tp <- sum(x$tested) / nrow(x) * 100
    msg <- sprintf('Package %s, Test Coverage: %.1f%%\n', pname, tp)


  # Functions
    dd  <- as.data.frame(x)
    dd$tested <- ifelse(x$tested, '+', '')


    funs <- list()

      dexp <- dd[dd$exp == TRUE, ]
      dexp <- dexp[order(dexp$fun), c('tested', 'fun')]
      names(dexp) <- c('', '')
      funs$exp <- dexp


      ds3 <- dd[dd$s3 == TRUE, ]
      ds3 <- ds3[order(ds3$fun), c('tested', 'fun')]
      names(ds3) <- c('', '')
      funs$s3 <- ds3


      dint <- dd[(dd$exp | dd$s3) == FALSE, ]
      dint <- dint[order(dint$fun), c('tested', 'fun')]
      names(dint) <- c('', '')
      funs$int <- dint


  # Print
    cat(msg, '\n')

    hline <- paste(rep('.', 20), collapse = '')

    if(nrow(funs$exp) > 0){
      cat(' exported functions', hline)
      print(funs$exp, row.names = FALSE, right = FALSE)
    }


    if(nrow(funs$s3) > 0){
      if(nrow(funs$exp) > 0){
        cat('\n')
      }
      cat(' S3 Methods', hline)
      print(funs$s3, row.names = FALSE, right = FALSE)
    }


    if(nrow(funs$int) > 0){
      if(nrow(funs$s3) > 0 || nrow(funs$exp) > 0){
        cat('\n')
      }
      cat(' internal functions', hline)
      print(funs$int, row.names = FALSE, right = FALSE)
    }


  invisible(x)
}




#' Get Test Coverage of Package
#'
#' This extracts the test coverage of the target package (usually the package
#' you are working on). Bear in mind that testthis uses a checklist-approach
#' for this, and depends that you either put the function name in your
#' \code{test_that} calls, or used test_this tags. If you want automatic
#' analysis of test coverage, you must look in other packages such as
#' \code{covr}.
#'
#' @param pkg path to package
#' @param from_tags Checks the files if your test directory for testthis tags.
#' Speicifically, if you have the comment \code{#* @testing myfunction} in any
#' of your test files, myfunction will be marked as tested.
#' @param from_desc Checks the \code{desc} argument \code{test_that(...)} of
#' the tests in your test directory for functions names. E.g. if you have a
#' testfile that contains \code{test_that("myfunction works"), ...}, myfunction
#' will be marked as tested.
#'
#' @export
get_test_coverage <- function(
  pkg = '.',
  from_tags = TRUE,
  from_desc = TRUE
){
  all  <- get_pkg_functions(pkg = pkg)
  tst  <- get_pkg_tested_functions(
    pkg = pkg,
    from_tags = from_tags,
    from_desc = from_desc
  )

  res <- data.frame(
    fun    = all,
    exp    = all %in% get_pkg_exports(pkg = pkg),
    s3     = all %in% get_pkg_S3methods(pkg = pkg),
    tested = all %in% tst,
    stringsAsFactors = FALSE
  )

  attr(res, 'package') <- devtools::as.package(pkg)$package
  test_coverage(res)
}
