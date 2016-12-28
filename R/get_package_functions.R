#' Get functions defined by package
#'
#' @param pgk
#'
#' @return
#' @import devtools
#'
#' @examples
get_package_functions <- function(pkg = '.'){
  pkg  <- devtools::as.package(pkg)
  res  <- as.character(unclass(lsf.str(envir = asNamespace(pkg$package), all = TRUE)))

  return(res)
}


