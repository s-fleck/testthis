get_tested_functions <- function(pkg, from_tags = TRUE, from_desc = TRUE){
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
  ttfuns  <- lapply(ttfiles, extract_testthis_tags)
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

  exps  <- unlist(lapply(ttfiles, parse))
  exps  <- exps[grep('test_that', as.list(exps))]
  descs <- extract_test_that_desc(exps)

  pkgfuns <- get_package_functions(pkg)
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


extract_test_that_desc <- function(dl){
  fun <- function(x) {
    # Tries to account for cases where test_that function arguments are
    # not in the right order
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

  lapply(dl, fun)
}
