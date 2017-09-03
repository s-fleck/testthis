
## Overview

Testthis provides several tools to make unit testing in R slightly more comfortable. It is designed to be used in conjunction with `testthat` and `devtools` packages.




## Rstudio addins (requires Rstudio)

Provides Rstudio addins that enable to assign hotkeys to menial tasks. I 
recommend assigning `ctrl+alt+Insert`, `ctrl+alt+Pos1` and `ctrl+alt+PageUp`.

* `test_this`: Tests the currently open file.
* `lest_this`: "Load and test"; first calls `devtools::load_all()`, and then 
  test the currently open file
* `open_tests`: Opens the associated testfile in an editor window. If the 
  currently open file already is a testfile, it opens the associated file in 
  the `/R` directory.
* `test_skeleton()` creates a testfile for the currently open file in the 
  recommended location (honors the testfile tag, see bellow).

The functions above assume that if the current filename is `currentfile.R`, the 
associated test file is `/tests/testthat/test_currentfile.R`. If you want to 
modify this behaviour you can put the tag 
`#* @testfile maybeasubdir/anotherfilename` anywhere in your code, usually the 
top or bottom of your .R file.

The following example will associate the file `/tests/testthat/other_test_file.R` 
instead of `/tests/testthat/test_open_tests.R` with `R/open_tests`:

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



## Infrastructure


testthis provides helpers to work with subdirectories of the `tests/testthis`
directory:

### testdata

* `use_testdata()` saves a file as the `.rds` file
* `read_testdata()` provides a shorthand for reading `.rds` files from 
  tests/testthis/tesdata`
* `use_testdata_raw()` creates the directory `tests/testthis/testdata-raw`.
  Scripts used to compute the datasets in `tests/testthis/testdata` go here.
      

**Examples:**

``` r
  use_testdata(iris)
  iris2 <- read_testdata("iris.rds")
```


### test subdir

Sometimes you will have long running tests, or tests that require external
resources that are not always available. You can put those tests in a subdir
of `tests/testthis`, so that `devtools::test()` will not run them automatically.
Testthis provides the following functions that make this all a bit more 
convenient:

* `test_subdir()` runs all tests in an arbitrary subdir of `tests/testthis`
* `use_test_subdir()` creates a new subdir in `tests/testthis`. If 
  `make_tester == TRUE` (the default) this will also create the R script file
  `R/testthis-testers.R` that contains a function defintion for running the 
  tests in the newly created directory (see example).
* `test_manual()`, `test_integration()` and `test_acceptance()` provide ready
  made helpers for running tests in the `manual_tests`, `integration_tests` and
  `acceptance_tests` subdirs for `test/testthis`. 
  
  
**Examples:**

``` r
use_test_subdir("nocran")

# reload package
# create test files in tests/testthat/nocran

test_nocran()

```




## Test coverage checklist

Testthis now provides a checklist like test coverage analyser. This is non-automatic and requires you to manually mark functions as tested in your code via special comments. If you look for automatic test coverage analysis, you might want to check out the `covr` package instead.

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
#> Package testthis, Test Coverage: 20.4%
#>  
#>  exported functions ....................                    
#>  - get_test_coverage
#>  - lest_this        
#>  - open_tests       
#>    read_test        
#>    save_test        
#>  - test_acceptance  
#>  - test_integration 
#>  - test_manual      
#>    test_skeleton    
#>  - test_subdir      
#>  - test_this        
#>  - test_with_skip   
#>  + use_testdata     
#>    use_testdata_raw 
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
#>  + get_pkg_tested_functions          
#>  + get_pkg_tested_functions_from_desc
#>    get_pkg_tested_functions_from_tags
#>    get_pkg_testfile_names_from_tags  
#>    get_pkg_testignore                
#>    get_rdir_taglist                  
#>  + get_tag                           
#>    get_taglist                       
#>    get_test_taglist                  
#>    get_testfile_name                 
#>    get_testfile_name_from_tag        
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
#>  + testdata_exists                   
#>  + testthis_tokenizer
```




## Testthis Tags

Test\_this tags are special comments that modify the behavious of the functions supplied by this package. They are of the Form `#* @tag <value>`. Please not that only some test\_this tags really require a `<value>`. The tag system should be considered to be in an alpha-state.

**`pkg/R/*.R`**

-   `@testfile <filename>`: manually specifiy associated test file. Should usually start with `test_`. This is used by `test_this()`, `lest_this()` and `open_tests()`.

**`pkg/tests/testthat/test_*.R`**

-   `@testing <functionname>`: mark `functionname` as tested. Should usually go next the associated `test_that()` call. This is used by `get_test_coverage()`.
-   `@skip`: skip test when running `testthis::test_with_skip()`. Best placed directly below the `context()` call.

also look at `/test/testthat/test_Taglist.R` for examples.



## Package options

You can set the following global options using `options()`:

-   `testthis.sep`: Default separator to use when creating test files with `test_skeleton()`. Defaults to `_`, must be either `_` or `-`. I.e whether you want your files to be named `test_foofunction.R` or `test-foofunction.R`
