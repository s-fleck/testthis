
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

  ttfiles <- list.files(testthat::test_path(), full.names = TRUE)
  ttfuns  <- lapply(ttfiles, extract_testthis_tags)
  ttfuns  <- unlist(ttfuns, recursive = FALSE)

  gtested <- function(x){
    if(identical(x[[1]], '@testing')){
      return(x[[2]])
    } else {
      return(NULL)
    }
  }

  ttfuns <- unlist(lapply(ttfuns, gtested))



}
