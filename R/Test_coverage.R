#' Get Test Coverage of Package
#'
#' This extracts the test coverage of the target package (usually the package
#' you are working on). Bear in mind that testthis uses a checklist-approach for
#' this, and depends that you either put the function name in your
#' `test_that()` calls, or used test_this tags. If you want automatic
#' analysis of test coverage, you must look in other packages such as `covr`.
#'
#' @template base_path
#' @param from_tags Logical scalar. Checks the files if your test directory for
#'   testthis tags. Specifically, if you have the comment `#* @testing myfunction`
#'   in any of your test files, myfunction will be marked as tested.
#' @param from_desc Logical scalar. Checks the `desc` argument
#'   `test_that(...)` of the tests in your test directory for functions
#'   names. E.g. if you have a testfile that contains
#'   `test_that("myfunction works", {...})`, myfunction will be marked as
#'   tested.
#'
#' @return A `Test_coverage` object. This is a `data.frame` containing the
#' following columns:
#'
#'   * fun: Name of the function
#'   * exp: Is function is exported?
#'   * s3: Is function an S3 method?
#'   * tested: Do unit tests exist for function?
#'   * ignore: Is function listed in \file{tests/testthat/_testignore}?
#'
#' @export
#' @examples
#'
#' \dontrun{
#' #' x <- get_test_coverage()
#' as.data.frame(x)  # To view in tabular form
#' }
#'
get_test_coverage <- function(
  base_path = '.',
  from_tags = TRUE,
  from_desc = TRUE
){
  all  <- get_pkg_functions(base_path = base_path)
  tst  <- get_pkg_tested_functions(
    base_path = base_path,
    from_tags = from_tags,
    from_desc = from_desc
  )
  ign <- get_pkg_testignore(base_path = base_path)


  res <- data.frame(
    fun    = all,
    exp    = all %in% get_pkg_exports(base_path = base_path),
    s3     = all %in% get_pkg_S3methods(base_path = base_path),
    tested = all %in% tst,
    ignore = all %in% ign,
    stringsAsFactors = FALSE
  )

  attr(res, 'package') <- devtools::as.package(base_path)$package
  test_coverage(res)
}




test_coverage <- function(dat){
  class(dat) <- c('Test_coverage', 'data.frame')

  assert_that(is_valid(dat))
  return(dat)
}




is_valid.Test_coverage <- function(dat){
  res <- list()

  res$names <- assert_that(identical(
    c('fun', 'exp', 's3', 'tested', 'ignore'),
    names(dat))
  )
  res$types <- assert_that(identical(
    unname(unlist(lapply(dat, class))),
    c('character', 'logical', 'logical', 'logical', 'logical'))
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
    dd$tested <- ""
    dd$tested[x$ignore] <- '-'
    dd$tested[x$tested] <- '+'

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




# utils -------------------------------------------------------------------

#' Get functions defined in target package
#'
#' Helper functions internally by used internally by [get_test_coverage()].
#'
#' @inheritParams get_test_coverage
#' @noRd
#' @return `get_pkg_functions()` returns a character vector of *all* functions
#'   defined in `base_path`.
#'
get_pkg_functions <- function(base_path = '.'){
  base_path  <- devtools::as.package(base_path)
  res  <- as.character(unclass(
    utils::lsf.str(
      envir = asNamespace(base_path$package),
      all = TRUE)
  ))
  return(res)
}




#' @rdname get_pkg_functions
#' @return `get_pkg_exports()` returns a character vector of functions *exported*
#'   from `base_path`s NAMESPACE.
#' @noRd
get_pkg_exports <- function(base_path = '.'){
  base_path %>%
    devtools::as.package() %>%
    devtools::parse_ns_file() %>%
    magrittr::extract2('exports')
}




#' @rdname get_pkg_functions
#' @return `get_pkg_S3methods()` returns a character vector of all *S3 methods*
#'   exported from `base_path`s NAMESPACE.
#' @noRd
get_pkg_S3methods <- function(base_path = '.'){
  dd <- base_path %>%
    devtools::as.package() %>%
    devtools::parse_ns_file() %>%
    magrittr::extract2('S3methods')

  apply(dd, 1, function(x) paste(stats::na.omit(x), collapse = '.'))
}




#' @rdname get_pkg_functions
#' @return  `get_pkg_tested_functions()` returns a character vector of all
#'   *functions for which unit tests exist*.
#' @noRd
get_pkg_tested_functions <- function(base_path, from_tags, from_desc){
  res <- vector()

  if(from_tags){
    res <- c(res, get_pkg_tested_functions_from_tags(base_path))
  }

  if(from_desc){
    res <- c(res, get_pkg_tested_functions_from_desc(base_path))
  }

  return(res)
}




#' @rdname get_pkg_functions
#' @return `get_pkg_testignore()` returns a character vector of all
#'   functions listed in \file{tests/testthat/_testignore}.
#' @noRd
get_pkg_testignore <- function(base_path){
  base_path <- devtools::as.package(base_path)
  tfile <- file.path(base_path$path, 'tests', 'testthat', '_testignore')

  if (file.exists(tfile)){
    return(readLines(tfile))
  } else {
    return(NULL)
  }
}




get_pkg_tested_functions_from_tags <- function(base_path){
  taglists <- get_test_taglist(base_path)
  res      <- sort(unlist(unique(lapply(taglists, get_tag, 'testing'))))

  return(res)
}




get_pkg_tested_functions_from_desc <- function(base_path){
  ttfiles <- list_test_files(base_path, full_names = TRUE)
  descs   <- extract_test_that_desc(ttfiles)

  pkgfuns <- get_pkg_functions(base_path)
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
#' @noRd
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
