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
  res  <- unclass(lsf.str(envir = asNamespace(pkg = pkg$package), all = TRUE))
}


#' Get functions defined in package
#'
#' @param pkg
#'
#' @return
#' @export
#'
#' @examples
get_tested_functions <- function(pkg = '.'){
  pkg       <- devtools::as.package(pkg)
  pkg_dir   <- base::system.file(package = pkg$package)

  pkgfuns <- get_package_functions(pkg)
}

