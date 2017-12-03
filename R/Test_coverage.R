#' Get Test Coverage of Package
#'
#' This extracts the test coverage of the target package (usually the package
#' you are working on). Bear in mind that testthis uses a checklist-approach for
#' this, and depends that you either put the function name in your
#' `test_that()` calls, or used test_this tags. If you want automatic
#' analysis of test coverage, you must look in other packages such as `covr`.
#'
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
#' x <- get_test_coverage()
#' as.data.frame(x)
#' }
#'
get_test_coverage <- function(
  from_tags = TRUE,
  from_desc = TRUE
){
  dat  <- get_pkg_tested_functions(
    from_tags = from_tags,
    from_desc = from_desc
  )
  funs <- names(dat)

  res <- data.frame(
    fun = funs,
    exp = funs %in% get_pkg_exports(),
    s3  = funs %in% get_pkg_S3methods(),
    tested =  vapply(dat, function(x) length(x) > 0, logical(1)),
    ignore = funs %in% get_pkg_testignore(),
    paths = I(unname(dat)),
    stringsAsFactors = FALSE
  )


  attr(res, "package") <- usethis::proj_get()
  test_coverage(res)
}




test_coverage <- function(dat){
  class(dat) <- c("Test_coverage", "data.frame")
  assert_that(is_valid(dat))
  return(dat)
}




is_valid.Test_coverage <- function(dat){
  res <- list()

  res$names <- assert_that(identical(
    c("fun", "exp", "s3", "tested", "ignore", "paths"),
    names(dat))
  )
  res$types <- assert_that(identical(
    unname(unlist(lapply(dat, class))),
    c("character", "logical", "logical", "logical", "logical", "AsIs"))
  )

  all(unlist(res))
}




#' @export
print.Test_coverage <- function(x, ...){

  # Heading
    pname <- attr(x, "package")
    if(is.null(pname)) pname <- ""
    tp <- sum(x$tested) / nrow(x) * 100
    msg <- sprintf("Package %s, Test Coverage: %.1f%%\n", pname, tp)


  # Functions
    dd  <- as.data.frame(x)
    dd$tested <- ""
    dd$tested[x$ignore] <- "-"
    dd$tested[x$tested] <- "+"

    funs <- list()

      dexp <- dd[dd$exp == TRUE, ]
      dexp <- dexp[order(dexp$fun), c("tested", "fun")]
      names(dexp) <- c("", "")
      funs$exp <- dexp


      ds3 <- dd[dd$s3 == TRUE, ]
      ds3 <- ds3[order(ds3$fun), c("tested", "fun")]
      names(ds3) <- c("", "")
      funs$s3 <- ds3


      dint <- dd[(dd$exp | dd$s3) == FALSE, ]
      dint <- dint[order(dint$fun), c("tested", "fun")]
      names(dint) <- c("", "")
      funs$int <- dint


  # Print
    cat(msg, "\n")

    hline <- paste(rep(".", 20), collapse = "")

    if(nrow(funs$exp) > 0){
      cat(" exported functions", hline)
      print(funs$exp, row.names = FALSE, right = FALSE)
    }


    if(nrow(funs$s3) > 0){
      if(nrow(funs$exp) > 0){
        cat("\n")
      }
      cat(" S3 Methods", hline)
      print(funs$s3, row.names = FALSE, right = FALSE)
    }


    if(nrow(funs$int) > 0){
      if(nrow(funs$s3) > 0 || nrow(funs$exp) > 0){
        cat("\n")
      }
      cat(" internal functions", hline)
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
#'   defined in package.
#'
get_pkg_functions <- function(){
  pkg  <- devtools::as.package(usethis::proj_get())

  ns <- tryCatch(
    asNamespace(pkg$package),
    error = function(e){
    stop(
      "Functions can only be detected for installed packages"
    )
  })

  res  <- as.character(unclass(
    utils::lsf.str(
      envir = asNamespace(pkg$package),
      all.names = TRUE)
  ))

  return(res)
}




#' @rdname get_pkg_functions
#' @return `get_pkg_exports()` returns a character vector of functions *exported*
#'   from the current packages NAMESPACE.
#' @noRd
get_pkg_exports <- function(){
  ns <- usethis::proj_get() %>%
    devtools::as.package() %>%
    devtools::parse_ns_file()

  if (identical(ns$exportPatterns, "^[[:alpha:]]+")){
    return(get_pkg_functions())
  } else {
    ns[["exports"]]
  }
}




#' @rdname get_pkg_functions
#' @return `get_pkg_S3methods()` returns a character vector of all *S3 methods*
#'   exported from the current packages NAMESPACE.
#' @noRd
get_pkg_S3methods <- function(){
  ns <- usethis::proj_get() %>%
    devtools::as.package() %>%
    devtools::parse_ns_file()

  if (identical(ns$exportPatterns, "^[[:alpha:]]+")){
    warning(
      "Detecting exported functions only works if all functions are ",
      "explicitly exported in the NAMESPACE file (for example via Roxygen)"
  )}

  ns %>%
    magrittr::extract2("S3methods") %>%
    apply(1, function(x) paste(stats::na.omit(x), collapse = "."))
}




#' @rdname get_pkg_functions
#' @return  `get_pkg_tested_functions()` returns a character vector of all
#'   *functions for which unit tests exist*.
#' @noRd
get_pkg_tested_functions <- function(from_tags, from_desc){

  funs <- get_pkg_functions()

  res <- vector("list", length(funs)) %>%
    setNames(funs)

  if(from_tags){
    dd <- get_pkg_tested_functions_from_tags()
    for(fun in names(dd)){
      res[[fun]] <- sort(dd[[fun]])
    }
  }

  if(from_desc){
    dd <- get_pkg_tested_functions_from_desc()
    for(fun in names(dd)){
      res[[fun]] <- sort(union(res[[fun]], dd[[fun]]))
    }
  }


  res <- res[names(res) %in% funs]
  assert_that(identical(length(res), length(funs)))
  res
}




#' @rdname get_pkg_functions
#' @return `get_pkg_testignore()` returns a character vector of all
#'   functions listed in \file{tests/testthat/_testignore}.
#' @noRd
get_pkg_testignore <- function(){
  tfile <- file.path(usethis::proj_get(), "tests", "testthat", "_testignore")

  if (file.exists(tfile)){
    return(readLines(tfile))
  } else {
    return(NULL)
  }
}



#' Guess tested functions based on contents of testthis tags
#'
#' @return A named `list` with on element per function of a package. The
#'   names of the list are the names of the functions, the values are
#'   `character` vectors of absolute file paths of test files that contain
#'   tests for the respective functions.
#' @noRd
#'
get_pkg_tested_functions_from_tags <- function(){
  taglists <- get_test_taglist()
  files <- lapply(taglists, get_tag, "testing")
  funs  <- unlist(files, use.names = FALSE)

  res <- vector("list", length(funs)) %>%
    setNames(funs)

  for(fl in names(files)){
    for(fun in files[[fl]]){
      res[[fun]] <- c(res[[fun]], fl)
    }
  }

  res
}




#' Guess tested functions based on contents of test_that desc argument
#'
#' @return A named `list` with on element per function of a package. The
#'   names of the list are the names of the functions, the values are
#'   `character` vectors of absolute file paths of test files that contain
#'   tests for the respective functions.
#' @noRd
#'
get_pkg_tested_functions_from_desc <- function(){
  ttfiles <- list_test_files(full_names = TRUE, recursive = TRUE)
  descs   <- extract_test_that_desc(ttfiles)
  pkgfuns <- get_pkg_functions()

  lapply(pkgfuns, function(.f){
    r <- lapply(seq_along(descs), function(i){
      is_tested <- grepl(pattern = .f, descs[[i]], fixed = TRUE)
      if (any(is_tested)) names(descs)[[i]] else invisible()
    })
    as.character(r[vapply(r, Negate(is.null), logical(1))])
  }) %>%
    setNames(pkgfuns)
}




#' Extract "desc" arguments from all test_that functions from .R script files
#'
#' @param infile character. Path to an .R script file, or a list of such paths;
#' usually created with list.files("/path/to/directory")
#' @return content of the "desc" arguments of test_that functions as a named
#'   list (one element per file, names correspond to full file paths.)
#' @noRd
extract_test_that_desc <- function(infile){
  exps  <- lapply(infile, parse) %>%
    setNames(infile)

  # fun tries to account for all possibilities where desc is not the second
  # argument of testthat
  fun <- function(x) {
    .x <- as.list(x)
    if("desc" %in% names(.x)){
      return(.x$desc)
    } else if ("code" %in% names(.x)){
      codepos <- which("code" == names(.x))
      if(identical(codepos, 2L)){
        return(.x[[3]])
      }
    } else {
      return(.x[[2]])
    }
  }


  lapply(exps, function(.x) {
    test_that_calls <- .x[grep("test_that", as.list(.x))]
    lapply(test_that_calls, fun)
  })
}
