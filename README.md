# Rstudio addins and utils for testthat

This is  a (very simple) package that provides utility function functions that I have been missing for my developing workflow and I thought I'd share. This package is aimed at people that use Rstudio together with the testthat package.

Assuming you currently have the file `my_fun.R` open in Rstudio:

* **test_this()** :  calls `testthat::test_file()` on *tests/testhat/test_my_fun.R*.
* **lest_this()** does the same, but calls `devtools::load_all()` first. ("load and test") 
* **test_skeleton()**:   Creates a file *tests/testthat/test_my_fun.R*
* **open_tests()**: opens *tests/testthat/test_my_fun.R* in a new tab 

All except `test_skeleton` work as R-studio addins; that means you can easily assign hotkeys to them (which was my main motivation behind creating this package)