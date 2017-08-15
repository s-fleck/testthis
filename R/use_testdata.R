#' Create \code{testdata} folder.
#'
#' Save R objects to separate files \file{tests/testthat/testdata} in the
#' `.rds` format.
#'
#' @param ... \R objects to save to the \file{testdata} dir. If empty,
#'   an empty directory is created.
#' @param subdir Character scalar. Subdirectory of \file{test_data} to save
#'   to / read from.
#' @inheritParams base::readRDS
#' @template overwrite
#' @inheritParams usethis::use_directory
#' @template pkg
#' @return `use_testdata()` returns `TRUE` if object was successfully saved.
#'
#' @section Side effects:
#'   `use_testdata()` saves an R object to a \file{testdata} dir in `pkg`.
#'
#' @export
#' @family infrastructure
#' @seealso [base::readRDS()]
#' @examples
#' \dontrun{
#'   use_testdata(letters, LETTERS)
#' }
use_testdata <- function(
  ...,
  subdir = NULL,
  overwrite = FALSE,
  ignore = FALSE,
  compress = TRUE,
  pkg = '.'
){
  # Preconditions
  assert_that(is.flag(overwrite))
  assert_that(is.null(subdir) || (is.scalar(subdir) && is.character(subdir)))
  assert_that(is.scalar(pkg) && is.character(pkg))


  # Find and prepare test_data directory
  base_path <- as.package(pkg)$path
  tdata_dir <- file.path("tests", "testthat", "testdata")
  if(!is.null(subdir)){
    tdata_dir <- file.path(tdata_dir, subdir)
  }
  save_path <- file.path(base_path, tdata_dir)

  show_dir_creation_message <- !dir.exists(save_path)
  usethis::use_directory(tdata_dir, ignore = FALSE, base_path = base_path)
  if(show_dir_creation_message){
    message(
      "* You can save data files for tests via `save_test()`\n",
      "* Scripts that produce test data should go in testdata-raw\n\n"
    )
  }


  # Save the files
    to_save     <- eval(substitute(alist(...)))
    if(identical(length(to_save), 0L))  return(invisible(TRUE))

    obj         <- vapply(to_save, as.character, character(1))
    save_files  <- paste0(file.path(save_path, obj), '.rds')

    existing_files <- save_files[file.exists(save_files)]

    if(!overwrite && length(existing_files) > 0L){
      msg <- sprintf(
        'Files already exist: \n%s',
        paste('*', existing_files, collapse = '\n')
      )
      stop(msg)
    }

    message(
      "Saving to \n", paste('*', save_files, collapse = "\n")
    )

    for(i in seq_along(save_files)){
      saveRDS(list(...)[[i]], file = save_files[i], compress = compress)
    }


  invisible(TRUE)
}




#' Create \code{testdata-raw} folder.
#'
#' A folder to put scripts in that produce the files in \file{testdata}
#'
#' @template pkg
#'
#' @export
#' @family infrastructure
use_testdata_raw <- function(pkg = "."){
  base_path <- as.package(pkg)$path
  usethis::use_directory(
    file.path("tests", "testthat", "testdata-raw"),
    ignore = FALSE,
    base_path = base_path
  )
  invisible(TRUE)
}




#' @rdname use_testdata
#'
#' @return `has_testdata()` returns `TRUE` if `pkg` has a
#' \file{tests/testthat/testdata} folder.
#'
has_testdata <- function(pkg = '.'){
  dir.exists(file.path(
    devtools::as.package(pkg)$path, "tests", "testthat", "testdata"
  ))
}




#' @param infile rds file to read (must end in .rds, otherwise .rds ending is
#'   automatically added)
#' @rdname use_testdata
#'
#' @return `read_testdata()` returns a single \R object
#' @export
read_testdata <- function(infile, subdir = NULL, pkg = '.'){
  # Preconditions
  assert_that(is.null(subdir) || (is.scalar(subdir) && is.character(subdir)))
  assert_that(is.scalar(pkg) && is.character(pkg))


  # Find test_data dir
  pkg       <- devtools::as.package(pkg)
  pkg_dir   <- base::system.file(package = pkg$package)
  cache_dir <- file.path(pkg_dir, 'tests', 'testthat', 'test_data')

  if(!is.null(subdir)){
    cache_dir <- file.path(cache_dir, subdir)
  }

  assert_that(file.exists(cache_dir))


  # Read file
  path        <- file.path(cache_dir, infile)
  if(!grepl('.*\\.rds$', path)){
    path <- paste0(path, '.rds')
  }

  readRDS(path)
}
