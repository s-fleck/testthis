#' Load/Save object to test_data directory
#'
#' @param ... R objects to save to / read from test data dir, usually \file{/tests/testthat/test_data/<subdir>}.
#'   Saving supports multiple files at once, read only works for single
#'   files.
#' @param pkg Package where to store data. Defaults to package in working directory.
#' @param subdir subdirectory of \file{test_data} to save to / read from.
#'
#' @section Side effects:
#'   Saves/reads an R object to a \file{test_data} dir in the current package
#'
#' @seealso \code{\link{load_cache}}.
#' @export
save_test <- function(..., pkg = '.', subdir = NULL, overwrite = FALSE){
  # Preconditions
    assert_that(is.flag(overwrite))
    assert_that(is.null(subdir) || (is.scalar(subdir) && is.character(subdir)))
    assert_that(is.scalar(pkg) && is.character(pkg))


  # Find and prepare test_data directory
    pkg       <- devtools::as.package(pkg)
    pkg_dir   <- base::system.file(package = pkg$package)                #base:: prevents devtools from inserting /inst/ into path
    cache_dir <- file.path(pkg_dir, 'tests', 'testthat', 'test_data')

    if(!is.null(subdir)){
      cache_dir <- file.path(cache_dir, subdir)
    }

    assert_that(file.exists(pkg_dir))

    if(!file.exists(cache_dir)){
      dir.create(cache_dir, recursive = TRUE)
    }

    assert_that(file.exists(cache_dir))


  # Save the files
    to_save     <- eval(substitute(alist(...)))
    obj         <- vapply(to_save, as.character, character(1))
    save_files  <- paste0(file.path(cache_dir, obj), '.rds')

    existing_files <- save_files[file.exists(save_files)]

    if(!overwrite && length(existing_files) > 0L){
      msg <- sprintf(
        'Files already exist: \n%s',
        paste('*', existing_files, collapse = '\n')
      )
      stop(msg)
    }

    message('Saving to \n', paste('*', save_files, collapse = '\n'))

    for(i in seq_along(save_files)){
      saveRDS(list(...)[[i]], file = save_files[i])
    }
}




#' @rdname save_test
#' @export
read_test <- function(..., pkg = '.', subdir = NULL){
  # Find test_data dir
    assert_that(is.null(subdir) || (is.scalar(subdir) && is.character(subdir)))
    assert_that(is.scalar(pkg) && is.character(pkg))


  # Find test_data dir
    pkg       <- devtools::as.package(pkg)
    pkg_dir   <- base::system.file(package = pkg$package)
    cache_dir <- file.path(pkg_dir, 'tests', 'testthat', 'test_data')

    if(!is.null(subdir)){
      cache_dir <- file.path(cache_dir, subdir)
    }

    assert_that(file.exists(pkg_dir))


  # Read file
    to_load     <- eval(substitute(alist(...)))
    obj         <- vapply(to_load, as.character, character(1))
    path        <- file.path(cache_dir, obj)
    if(!grepl('.*\\.rds$', path)){
      path <- paste0(path, '.rds')
    }

    readRDS(path)
}
