#' Get all functions defined in  a package
#'
#' @param pgk path to the package
#'
#' @return a character vector
#' @import devtools
#' @export
get_package_functions <- function(pkg = '.'){
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
#' @export
get_exported_functions <- function(pkg = '.'){
  pkg <- devtools::as.package(pkg)

  ns  <- readLines(file.path(pkg$path, 'NAMESPACE'))
  exp <- stringi::stri_extract(ns,
                               regex = "(?<=export\\().*(?=\\))",
                               simplify = TRUE)
  exp <- as.character(na.omit(exp))
}
