
testthis
========

Overview
--------

Testhis provides RStudio addins for common package development tasks:

-   `test_this()`: Run tests associated with the currently open R script file.
-   `lest_this()`: "Load and test"; As above, but call `devtools::load_all()` first
-   `test_with_skip()`: Like `devtools::test()`, but does not run test files that contain the line `#' @skip.`
-   `open_testfile()`: Opens the associated testfile in an editor window. If the currently open file already is a testfile, it opens the associated file in the `/R` directory. Can be used to jump back and forth between both.

It furhter provides functions for managing subdirectories of the `tests/testthat` directory of a package:

-   `use_testdata()` places a single R object in the `tests/testhat/testdata` directory. It is analoguous to `usethis::use_data()`, except that it saves the Object in the `.rds` format, which is more convenient for single R Objects than `.rda` or `.Rdata` (see `?readRDS`).
-   `use_testdata_raw()` creates the directory `tests/testhat/testdata-raw`. Use this directory to put scripts that generate the data in `tests/testhat/testdata`.
-   `read_testdata()` is a simple wrapper for `readRDS()` to read files in `tests/testhat/testdata`.
-   `use_test_subdir()` and `test_subdir()` for putting/running tests in subdirectories of `tests/testhat/`. These tests will *not* be run on CRAN or by `devtools::test()`.

Testthis also provides the experimental feature `get_test_coverage()`. This displays the test coverage of a package, but in contrast the [covr](https://github.com/r-lib/covr) the test coverage is not calculated automatically. To work, `get_test_coverage()` requires you to put special comment tags in your test-source files. Please see the package vignette for examples.

Installation
------------

testthis is CRAN-ready, but not yet on CRAN. This is beause it depends on the r-lib package usethis, which is also not yet on CRAN. You can install testthis like this:

    # install.packages("devtools")
    devtools::install_github("r-lib/usethis")
    devtools::install_github("s-fleck/testthis")
