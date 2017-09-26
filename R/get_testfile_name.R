#' Get testfile name
#'
#' Get the name of a test associated with target R script
#'
#' @param sep Default separator to use when creating test files, i.e whether
#'   you want your files to be named `test_foofunction.R` or
#'   `test-foofunction.R`
#'
#' @return If the currently open file:
#' * Is a normal R script: the path to the associated test file, usually
#'   \file{tests/testthat/test_currentlyopenfile.R}. An alternative name for
#'   the test file can be specified via the special comment tag
#'   `#* @testfile <filename>`.
#'
#' * Is a test file (i.e. its name starts with `test_` or
#'   `test-`): the path to the currently open file.
#' @noRd
#'
get_testfile_name <- function(sep = options('testthis.sep')){

  # Preconditions
    assert_that(is.scalar(sep) && sep %in% c("-", "_"))


  # Logic
    fname <- get_current_file()
    testfile_name  <- get_testfile_name_from_tag(fname)$tfile

    if (identical(length(testfile_name), 0L)){  # No @testfile tag found

      if(is_testfile(fname) && !is_in_rdir(fname)) return(fname)

      # Look for exisiting test file that matches testfile naming pattern
        bn <- basename(fname)
        test_pattern <- sprintf('^test[_\\-]')
        test_bn_pattern <- paste0(test_pattern, bn)

        testfile_name <- testthat::test_path() %>%
          list.files() %>%
          stringi::stri_subset_regex(test_bn_pattern) %>%
          file.path(testthat::test_path(), .)

      # If no match is found, construct a new testfile name
        if (identical(length(testfile_name), 0L)){
          testfile_name <- file.path(
            testthat::test_path(),
            paste0('test', sep, bn)
          )
        }

    } else {  # An @testfile tag was found
      testfile_name <- file.path(testthat::test_path(), testfile_name)
    }


  # Post processing
    if (length(testfile_name) > 1) {
      warning(
        'More than one possible test file found. Using ',
        testfile_name[[1]]
      )

      testfile_name <- testfile_name[[1]]
    }


  # Post conditions
    if(!is_testfile(testfile_name)){
      stop(
        testfile_name,
        ' is not a valid name for a test file. ',
        'Test file names must start with either "test_" or "test-".'
      )
    }


  return(testfile_name)
}
