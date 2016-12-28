#' Get functions defined by package
#'
#' @param pgk
#'
#' @return
#' @import devtools
#'
#' @examples
get_package_functions <- function(pgk = '.'){
  pkg  <- devtools::as.package(pkg)
  res  <- unclass(lsf.str(envir = asNamespace(pkg$package), all = TRUE))
}


