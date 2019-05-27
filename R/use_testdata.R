#' Create \code{testdata} folder.
#'
#' Save R objects to separate files \file{tests/testthat/testdata} in the
#' `.rds` format.
#'
#' @param ... \R objects to save to the \file{testdata} dir. If empty,
#'   an empty directory is created.
#' @param subdir `character` scalar. Subdirectory of \file{testdata} to save
#'   to / read from.
#' @inheritParams base::readRDS
#' @template overwrite
#' @inheritParams usethis::use_directory
#' @return `use_testdata()` returns `TRUE` if object was successfully saved.
#'
#' @section Side effects:
#'   `use_testdata()` saves an R object to a \file{testdata} dir in the current package.
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
  refhook = NULL,
  version = NULL
){
  # Preconditions
  assert_that(
    is_scalar_bool(overwrite),
    is_scalar_bool(ignore),
    is.null(subdir) || is_scalar_character(subdir)
  )


  # Find and prepare testdata directory
  pkg <- usethis::proj_get()
  tdata_dir <- file.path("tests", "testthat", "testdata")

  if(!is.null(subdir)){
    tdata_dir <- file.path(tdata_dir, subdir)
  }
  save_path <- file.path(pkg, tdata_dir)

  show_dir_creation_message <- !dir.exists(save_path)
  usethis::use_directory(tdata_dir, ignore = FALSE)
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
      saveRDS(
        list(...)[[i]],
        file = save_files[i],
        compress = compress,
        version = version,
        refhook = refhook
      )
    }


  invisible(TRUE)
}




#' `use_testdata_raw()` creates a folder for scripts that produce the files in
#' \file{testdata}
#' @rdname use_testdata
#' @export
use_testdata_raw <- function(){
  usethis::use_directory(
    file.path("tests", "testthat", "testdata-raw"),
    ignore = FALSE
  )
  invisible(TRUE)
}




#' @rdname use_testdata
#' @return `has_testdata()` returns `TRUE` if package has a
#' \file{tests/testthat/testdata} folder.
#'
has_testdata <- function(){
  dir.exists(file.path(
    usethis::proj_get(), "tests", "testthat", "testdata"
  ))
}




#' @param infile rds file to read (must end in .rds, otherwise .rds ending is
#'   automatically added)
#' @rdname use_testdata
#'
#' @return `read_testdata()` returns a single \R object
#' @export
read_testdata <- function(infile, subdir = NULL){
  # Preconditions
  assert_that(is.null(subdir) || (is.scalar(subdir) && is.character(subdir)))

  # Find testdata dir
  path <- tryCatch(usethis::proj_get(), error = function(e) ".")

  if(is.null(subdir)){
    cache_dir <- find_testdata(must_exist = TRUE, path = path)
  } else {
    cache_dir <- find_testdata(subdir, must_exist = TRUE, path = path)
  }
  assert_that(file.exists(cache_dir))

  # Read file
  path  <- file.path(cache_dir, infile)
  if(!grepl('.*\\.rds$', path)){
    path <- paste0(path, '.rds')
  }

  readRDS(path)
}




#' @inheritParams rprojroot::find_testthat_root_file
#' @param must_exist `logical` scalar. Assert that path specified in `...`
#'   exists
#'
#' @return `find_testdata()` returns the normalized path to a file in a
#'   in the testdata directory
#'
#' @export
#' @rdname use_testdata
find_testdata <- function(..., path = ".", must_exist = FALSE){
  p <- rprojroot::find_testthat_root_file("testdata", ..., path = path)
  if (must_exist){
    assert(file.exists(p), "Could not find '", p, "'")
  }
  p
}
