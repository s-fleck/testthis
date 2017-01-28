# Rstudio addins and utils for testthat

Testthis provides several tools to make unit testing in R slightly more 
confortable. It is designed to be used in conjunction with `testthat` and 
`devtools` packages. 

## Rstudio Addins:

Assuming you currently have the file `my_fun.R` open in Rstudio:

* **test_this()** :  calls `testthat::test_file()` on *tests/testhat/test_my_fun.R*.
* **lest_this()** does the same, but calls `devtools::load_all()` first. ("load and test") 
* **open_tests()**: opens *tests/testthat/test_my_fun.R* in a new tab. If it does
  not exist a new file is created.

All these functions are registered as Rstudio addins, that means you can easily 
assign hotkeys to them (which was my main motivation behind creating this 
package)


## Coverage:

Testhis now provides a checklist like test coverage analyser. 

`get_test_coverage()`


    Package testthis, Test Coverage: 31.8%
     
     exported functions ....................                   
      get_test_coverage
      lest_this        
      open_tests       
      test_this        
    
     internal functions ....................                                 
     + detect_testthis_comments      
       extract_test_that_desc        
     + extract_testthis_comments     
       get_all_functions             
       get_exported_functions        
     + get_tag                       
     + get_taglist                   
     + get_tested_functions          
     + get_tested_functions_from_desc
       get_tested_functions_from_tags
       get_testfile_name             
       is_valid                      
       is_valid.Test_coverage        
       print.Test_coverage           
       taglist                       
       test_coverage                 
       test_skeleton                 
     + testthis_tokenizer        


# More information:
A more detailed description can be found in the package vignette, 
vignette("testthis", package = "testthis")
