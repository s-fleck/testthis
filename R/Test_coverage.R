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

#' Get Test Coverage of Package
#'
#' @param pkg path to package
#'
#' @param ...
#'
#' @export
get_test_coverage <- function(pkg = '.', from_tags = TRUE, from_desc = TRUE){
  all  <- get_all_functions(pkg = pkg)
  tst  <- get_tested_functions(pkg = pkg,
                               from_tags = from_tags,
                               from_desc = from_desc)
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


# Package functions -------------------------------------------------------

#' Get all functions defined in  a package
#'
#' @param pgk path to the package
#'
#' @return a character vector
#' @import devtools
get_all_functions <- function(pkg = '.'){
  pkg  <- devtools::as.package(pkg)
  res  <- as.character(unclass(lsf.str(envir = asNamespace(pkg$package), all = TRUE)))
  return(res)
}


#' Get exported functions of a package
#'
#' @param pgk path to the package
#'
#' @return a character vector
#' @import devtools
get_exported_functions <- function(pkg = '.'){
  pkg <- devtools::as.package(pkg)

  ns  <- readLines(file.path(pkg$path, 'NAMESPACE'))
  exp <- stringi::stri_extract(ns,
                               regex = "(?<=export\\().*(?=\\))",
                               simplify = TRUE)
  exp <- as.character(na.omit(exp))
}


# tested functions --------------------------------------------------------

get_tested_functions <- function(pkg, from_tags, from_desc){
  res <- c()

  if(from_tags){
    res <- c(res, get_tested_functions_from_tags(pkg))
  }

  if(from_desc){
    res <- c(res, get_tested_functions_from_desc(pkg))
  }

  return(res)
}


# devtools::load_all('/home/hoelk/Dropbox/workspace/r/testthis/testthis')
#' Get functions defined in package
#'
#' @param pkg
#'
#' @return
#'
#' @examples
get_tested_functions_from_tags <- function(pkg){
  pkg     <- devtools::as.package(pkg)
  tpath   <- system.file('tests', 'testthat', package = pkg$package, mustWork = TRUE)

  ttfiles <- list.files(tpath, full.names = TRUE)
  ttfuns  <- lapply(ttfiles, get_taglist)
  ttfuns  <- unlist(ttfuns, recursive = FALSE)

  gtested <- function(x){
    if (identical(x[[1]], '@testing')){
      return(x[[2]])
    } else {
      return(NULL)
    }
  }

  ttfuns <- unique(unlist(lapply(ttfuns, gtested)))
}


get_tested_functions_from_desc <- function(pkg){
  pkg       <- devtools::as.package(pkg)
  pkg_dir   <- base::system.file(package = pkg$package)

  ttfiles <- list.files(testthat::test_path(), full.names = TRUE)
  descs   <- extract_test_that_desc(ttfiles)

  pkgfuns <- get_all_functions(pkg)
  res <- rep(NA, length(pkgfuns))

  for(i in seq_along(pkgfuns)){
    res[[i]] <- any(
      stringi::stri_detect_fixed(
        descs,
        pattern = pkgfuns[[i]]
      )
    )
  }

  assert_that(identical(length(res), length(pkgfuns)))
  assert_that(!any(is.na(res)))

  return(pkgfuns[res])
}


#' Extract 'desc' arguments from all test_that functions from .R script files
#'
#' @param infile character. Patht to an .R script file, or a list of such paths;
#' usually created with list.files("/path/to/directory")
#' @return content of the 'desc' arguments of test_that functions
extract_test_that_desc <- function(infile){
  exps  <- unlist(lapply(infile, parse))
  exps  <- exps[grep('test_that', as.list(exps))]

  # fun tries to account for all possibilities where desc is not the second
  # argument of testthat
  fun <- function(x) {
    .x <- as.list(x)
    if('desc' %in% names(.x)){
      return(.x$desc)
    } else if ('code' %in% names(.x)){
      codepos <- which('code' == names(.x))
      if(identical(codepos, 2L)){
        return(.x[[3]])
      }
    } else {
      return(.x[[2]])
    }
  }

  lapply(exps, fun)
}

