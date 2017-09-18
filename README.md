
Overview
--------

Testthis provides tools to make unit testing in R more comfortable. It is designed to complement the packages `testthat`, `devtools` and `usethis`.

Rstudio addins
--------------

Provides Rstudio addins for common tasks, that can also be assigned to hotkeys. I recommend assigning `ctrl+alt+Insert`, `ctrl+alt+Pos1` and `ctrl+alt+PageUp` to the following functions[1]:

-   `test_this()`: Tests the currently open file.
-   `lest_this()`: "Load and test"; first calls `devtools::load_all()`, and then test the currently open file
-   `open_tests()`: Opens the associated testfile in an editor window. If the currently open file already is a testfile, it opens the associated file in the `/R` directory.

The function above assume that if the current filename is `currentfile.R`, the associated test file is `/tests/testthat/test_currentfile.R`. If you want to modify this behaviour you can put the tag `#* @testfile anotherfile` anywhere in your code, usually the top or bottom of your .R file.

The following example will associate the file `/tests/testthat/other_test_file.R` instead of `/tests/testthat/test_open_tests.R` with `R/open_tests`:

``` r
# file R/open_tests.R
#* @testfile other_testfile

open_tests <- function(){
  fname <- get_testfile_name()

  if(file.exists(fname)){
    rstudioapi::navigateToFile(fname)
  } else {
    test_skeleton(fname, open = TRUE)
  }
}
```

Managing test data
------------------

Sometimes, it might become necessary to have access to complex data sets for tests. The recommended way for dealing with this is to provide an example dataset in your packages `data/` directory; however, there are also usecases where you want seperate data files for your tests.

Testthis provides three functions to to make dealing with separate test data files a bit easier:

-   `use_testdata()` places a single R object in the `tests/testhat/testdata` directory. It is analoguous to `usethis::use_data()`, except that it saves the Object in the `.rds` format, which is more convenient for single R Objects than `.rda` or `.Rdata` (see `?readRDS`).
-   `use_testdata_raw()` creates the directory `tests/testhat/testdata-raw`. Use this directory to put scripts that generate the data in `tests/testhat/testdata`.
-   `read_testdata()` is a simple wrapper for `readRDS()` to read files in `tests/testhat/testdata`.

Managing test subdirectories
----------------------------

Sometimes it is useful to create tests for a package that go beyond simple best-practice unit tests. For example you might have some tests that require external ressources (web, databases) or take long to execute. If you put such tests in subdirectories of `test/testthat` they will neither be automaticaly executed by `devtools::test()` nor run on CRAN.

testthis provides helpers do deal with files in subdirectories of `tests/testthat`.

-   `use_test_subdir()` create a subdir in `tests/testthat/` and (optionally) an R script in `/R` that contains a helper function to run all tests in that subdir. By default, said .R script will be added to your packages `.Rbuildignore` as it is only useful during package development.
-   You can also just manually use `test_subdir("mysubdir")` to run tests in a subdir of `tests/testthat`.
-   `test_acceptance()`, `test_manual()` and `test_integration()` are presets to run tests in the `integration_tests`, `acceptance_tests` and `manual_tests` subdirectories of `test/testthat`.

Test coverage checklist
-----------------------

**This is an experimental feature, comments and feature request are welcome** Testthis now provides a checklist like test coverage analyser. This is non-automatic and requires you to manually mark functions as tested in your code via special comments. If you look for automatic test coverage analysis, you might want to check out the `covr` package instead.

For testthis to recognize wheter a unit test exists for a function, the function name must either be mention in a `testthat()` `desc` argument, or marked via special comment tag: `#* @testing functionname`. The following example will mark the functions `parse_testthis_comments` and `detect_testthis_comments` as tested:

``` r
# file /tests/testhis/parse_testthis_comments.R

test_that("parse_testthis_comments works as expected", {
  #* @testing detect_testthis_comments
  ...
}
```

The test\_coverage summary for a package may look like this (+ marks function for which tests exist):

``` r
get_test_coverage(pkg = system.file(package = 'testthis'))
#> Package testthis, Test Coverage: 15.4%
#>  
#>  exported functions ....................                    
#>  - get_test_coverage
#>  - lest_this        
#>  - open_tests       
#>    read_testdata    
#>  - test_acceptance  
#>  - test_integration 
#>  - test_manual      
#>    test_skeleton    
#>  - test_subdir      
#>  - test_this        
#>  - test_with_skip   
#>    use_test_subdir  
#>  + use_testdata     
#>    use_testdata_raw 
#>    use_tester       
#> 
#>  S3 Methods ....................                     
#>   print.Test_coverage
#> 
#>  internal functions ....................                                     
#>    .onLoad                           
#>  + detect_testthis_comments          
#>    extract_test_that_desc            
#>  + extract_testthis_comments         
#>    get_current_file                  
#>    get_pkg_exports                   
#>    get_pkg_functions                 
#>    get_pkg_S3methods                 
#>    get_pkg_tested_functions          
#>    get_pkg_tested_functions_from_desc
#>    get_pkg_tested_functions_from_tags
#>    get_pkg_testfile_names_from_tags  
#>    get_pkg_testignore                
#>    get_rdir_taglist                  
#>  + get_tag                           
#>    get_taglist                       
#>    get_test_taglist                  
#>    get_testfile_name                 
#>    get_testfile_name_from_tag        
#>  + has_testdata                      
#>    is_in_rdir                        
#>    is_testfile                       
#>    is_testfiles                      
#>    is_valid                          
#>    is_valid.Test_coverage            
#>    list_rdir_files                   
#>  + list_test_files                   
#>    msg_testfile_does_not_exist       
#>    open_associated_rfile             
#>    require_rstudio                   
#>  + skip_test_files                   
#>    taglist                           
#>    test_coverage                     
#>  + testthis_tokenizer                
#>    use_acceptance_tests              
#>    use_integration_tests
```

Testthis Tags
-------------

Test\_this tags are special comments that modify the behavious of the functions supplied by this package. They are of the Form `#* @tag <value>`. Please not that only some test\_this tags really require a `<value>`. The tag system should be considered to be in an alpha-state.

### `pkg/R/*.R`

-   `@testfile <filename>`: manually specifiy associated test file. Should usually start with `test_`. This is used by `test_this()`, `lest_this()` and `open_tests()`.

### `pkg/tests/testthat/test_*.R`

-   `@testing <functionname>`: mark `functionname` as tested. Should usually go next the associated `test_that()` call. This is used by `get_test_coverage()`.
-   `@skip`: skip test when running `testthis::test_with_skip()`. Best placed directly below the `context()` call.

also look at `/test/testthat/test_Taglist.R` for examples.

Package options
---------------

You can set the following global options using `options()`:

-   `testthis.sep`: Default separator to use when creating test files with `test_skeleton()`. Defaults to `_`, must be either `_` or `-`. I.e whether you want your files to be named `test_foofunction.R` or `test-foofunction.R`

[1] you can do so under *Tools/Modify Keyboard Shortcuts*
