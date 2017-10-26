
testthis
========

[![CRAN status](http://www.r-pkg.org/badges/version/testthis)](https://cran.r-project.org/package=testthis)

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

Testthis also provides the experimental feature `get_test_coverage()`. In contrast to the popular [covr](https://github.com/r-lib/covr) package, this does *not* calculate the coverage automatically. It rather requires you to put special comment tags in your test-source files. You can use it like a todo/checklist.

Installation
------------

    # Testthis is on CRAN:
    install.packages("testthis")

    # You can also install the development version from GitHub:
    # install.packages("devtools")
    devtools::install_github("s-fleck/testthis")

Usage
-----


    library(testthis)

    open_testfile()  # can be assigned to a hotkey
    ## The file `tests/testthat/test_README.Rmd` does not exist. 
    ## You can create it with testthis::test_skeleton().

    test_skeleton()
    ## * Creating `tests/testthat/test_myscript.R`

    open_testfile()
    ## > Opens `tests/testthat/test_myscript.R` (in RStudio)

    test_this()  # can be assigned to a hotkey
    ## > Runs tests in `tests/testthat/test_myscript.R` (in RStudio,)
    ## > works from the original .R file as well as from the file containing the tests

For a detailed overview of all testthis features with usage examples please refer to the package [vignette](http://rpubs.com/hoelk/testthis).
