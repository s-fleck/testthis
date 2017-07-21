
#' Get all functions defined in target package
#'
#' @param pkg path to the package
#'
#' @return a character vector
#' @import devtools
get_pkg_functions <- function(pkg = '.'){
  pkg  <- devtools::as.package(pkg)
  res  <- as.character(unclass(
    utils::lsf.str(
      envir = asNamespace(pkg$package),
      all = TRUE)
  ))
  return(res)
}




#' Get exported functions of a package
#'
#' Lists the functions exported by a package (according to the NAMESPACE file)
#'
#' @inheritParams get_test_coverage
#'
#' @return a character vector
get_pkg_exports <- function(pkg = '.'){
  pkg %>%
    devtools::as.package() %>%
    devtools::parse_ns_file() %>%
    magrittr::extract2('exports')
}







#' Get exported functions of a package
#'
#' Lists the functions exported by a package (according to the NAMESPACE file)
#'
#' @inheritParams get_test_coverage
#'
#' @return a character vector
get_pkg_S3methods <- function(pkg = '.'){
  dd <- pkg %>%
    devtools::as.package() %>%
    devtools::parse_ns_file() %>%
    magrittr::extract2('S3methods')

  apply(dd, 1, function(x) paste(stats::na.omit(x), collapse = '.'))
}




# Utils -------------------------------------------------------------------

#' Get tested functions of a package
#'
#' @inheritParams get_test_coverage
#' @return a character vector
get_pkg_tested_functions <- function(pkg, from_tags, from_desc){
  res <- vector()

  if(from_tags){
    res <- c(res, get_pkg_tested_functions_from_tags(pkg))
  }

  if(from_desc){
    res <- c(res, get_pkg_tested_functions_from_desc(pkg))
  }

  return(res)
}




#' Get tested functions of a package
#'
#' @inheritParams get_test_coverage
#' @return a character vector
get_pkg_testignore <- function(pkg){
  pkg <- devtools::as.package(pkg)
  tfile <- file.path(pkg$path, 'tests', 'testthat', '.testthisignore')

  if (file.exists(tfile)){
    return(readLines(tfile))
  } else {
    return(NULL)
  }
}



get_pkg_tested_functions_from_tags <- function(pkg){
  taglists <- get_test_taglist(pkg)
  res      <- sort(unlist(unique(lapply(taglists, get_tag, 'testing'))))

  return(res)
}




get_pkg_tested_functions_from_desc <- function(pkg){
  ttfiles <- list_test_files(pkg, full_names = TRUE)
  descs   <- extract_test_that_desc(ttfiles)

  pkgfuns <- get_pkg_functions(pkg)
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
#' @param infile character. Path to an .R script file, or a list of such paths;
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
