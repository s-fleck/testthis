#' Use data in a package.
#'
#' This function makes it easy to save package data in the correct format.
#'
#' @param ... Unquoted names of existing objects to save.
#' @param pkg Package where to store data. Defaults to package in working
#'   directory.
#' @param overwrite By default, \code{use_data} will not overwrite existing
#'   files. If you really want to do so, set this to \code{TRUE}.
#' @inheritParams base::readRDS
#'
#' @export
#' @family infrastructure
#' @examples
#' \dontrun{
#' x <- 1:10
#' y <- 1:100
#'
#' use_data(x, y) # For external use
#' use_data(x, y, internal = TRUE) # For internal use
#' }
use_testdata <- function(
  ...,
  pkg = ".",
  internal = FALSE,
  overwrite = FALSE,
  compress = "bzip2"
){
  pkg  <- as.package(pkg)
  objs <- eval(substitute(alist(...)))

  # Prepare / check output paths
    dir_name <- file.path(pkg$path, "tests", "testthat", "testdata")
    if (!file.exists(dir_name)) {
      use_directory(
        path = file.path("tests", "testthat", "testdata"),
        ignore = TRUE,
        pkg = pkg
      )
    }

    paths <- file.path(dir_name, paste0(objs, ".rds"))
    if(!overwrite & any(file.exists(paths))){
      existing <- paste(basename(paths[file.exists(paths)]), collapse = ', ')
      msg <- sprintf(
        "%s already exist in %s and overwrite == FALSE",
        existing, dir_name
      )
      stop(msg)
    }

  # Save
    message(sprintf(
      "Saving %s to %s",
      paste(basename(paths), collapse = ", "),
      dir_name
    ))

    mapply(
      saveRDS,
      list(...),
      file = paths,
      MoreArgs = list(compress = compress)
    )

  invisible()
}



#' Use \code{testdata-raw} to compute package datasets.
#'
#' @param pkg Package where to create \code{testdata-raw}. Defaults to package in
#'   working directory.
#' @export
#' @family infrastructure
use_testdata_raw <- function(pkg = ".") {
  pkg <- as.package(pkg)

  use_directory(file.path("tests", "testthat", "testdata-raw", ignore = FALSE, pkg = pkg))

  message(
    "Next: \n",
    "* Put scripts that produce testsdata in tests/testthat/testdata-raw\n",
    "* Use testthis::use_testdata() to add testdata to package")
}



# cited from devtools
use_directory <- function(
  path,
  ignore = FALSE,
  pkg = "."
){
  pkg <- as.package(pkg)
  pkg_path <- file.path(pkg$path, path)

  if (file.exists(pkg_path)) {
    if (!file.info(pkg_path)$isdir) {
      stop("`", path, "` exists but is not a directory.", call. = FALSE)
    }
  } else {
    message("* Creating `", path, "`.")
    dir.create(pkg_path, showWarnings = FALSE, recursive = TRUE)
  }

  if (ignore) {
    message("* Adding `", path, "` to `.Rbuildignore`.")
    devtools::use_build_ignore(path, pkg = pkg)
  }

  invisible(TRUE)
}


dots <- function(...) {
  eval(substitute(alist(...)))
}
