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
#' @template pkg
#' @return `use_testdata()` returns `TRUE` if object was succesfully saved.
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




#' Use test subdir
#'
#' Create a subdir in \file{tests/testthat/} and optionally an R script
#' containing a helper function to run all tests in that subdir. Useful for
#' sepparating long-running tests from your unit tests.
#'
#' @param path Character scalar. Will be processed with [base::make.names()] to
#'   make a syntactically valid name.
#' @param make_tester Logical or character scalar. Create an R script with a
#'   test helper function. If `TRUE` an R script file will be placed into the
#'   \file{R/} directory of \file{pkg}, containing a function definition
#'   for running the tests in `path`. The file will be named
#'   \file{testthis-testers.R}, but you can specify  your own name by
#'   passing a character scalar to make_tester. See [use_tester()] for details.
#' @template pkg
#' @family infrastructure
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' use_test_subdir("special_tests", make_tester = TRUE)
#'
#' ## Reload the Package manually...
#' ## Create some tests in tests/testthat/test_special_tests/
#'
#' test_special_tests()
#' }
#'
use_test_subdir <- function(
  path,
  make_tester = TRUE,
  pkg = "."
){
  # Preconditions
    assert_that(is.scalar(path) && is.character(path))
    assert_that(is.scalar(make_tester))
    assert_that(is.scalar(is.logical(make_tester) || is.character(make_tester)))


  # Process arguments
    path <- make.names(path)
    base_path <- devtools::as.package(pkg)$path


  # Logic
    usethis::use_directory(
      file.path("tests", "testthat", path),
      ignore = FALSE,
      base_path = base_path
    )

    if (is.character(make_tester)) {
      use_tester(path, tester_path = make_tester, pkg = pkg)
    } else if (make_tester) {
      use_tester(path, pkg = pkg)
    }


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




#' Use a tester function
#'
#' Quickly create an \R script that contains a function for running all tests
#' in a predifined directory. This function powers the `make_tester` option
#' of [use_test_subdir()] and you will likely not need to run it manually.
#'
#' @param path Name of the subdirectory oft \file{tests/testthat/} for which
#'   to create a tester function.
#' @param ignore Logical. Add `tester_path` to .Rbuildignore?
#' @param tester_path \R script file in which to store the tester functions
#' @template pkg
#'
#' @return `TRUE` on success
#' @export
#' @family infrastructure
#'
use_tester <- function(
  path,
  ignore = false,
  tester_path = file.path("R", "testthis-testers.R"),
  pkg = "."
){
  fname <- file.path(devtools::as.package(pkg)$path, tester_path)
  funname   <- paste0("test_", path)

  message(sprintf("creating tester function %s() in %s", funname, fname))
  rcode <- sprintf('%s <- function() testthis::test_subdir("%s")\n', funname, path)

  write(rcode, fname, append = TRUE)
  invisible(TRUE)
}




# Utils -------------------------------------------------------------------

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

