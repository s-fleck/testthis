# testthis 1.1.1

* `use_testdata_raw()` gains `name` argument similar to `usethis::use_data()`.
  If `name` is provided, a script file for a dataset of that name is created
  (and opened if you are using RStudio) (#15, thanks @TylerGrantSmith)
* removed some brittle tests for compatibility with usethis 1.6.0 (#16, thanks @jennybc)
* don't load helpers twice in `test_this()` (#13, thanks @krlmlr)


# testthis 1.1.0

* added `find_testdata()` which returns the path to the testdata directory
* added `version` and `refhook` argument to `use_testdata()` (which are passed
  on to `saveRDS()`). You can use `version = 2` to create testdata that is
  compatible with R < 3.5.0.
* added `test_index()` which displays an index / table of contents of all 
  `test_that()` calls in a package's `tests/testthat` directory. Uses
  source markers if called from RStudio.
* renamed `get_test_coverage()` to just `test_coverage()`
* added `.covrignore` support to `test_coverage()`
* `read_testdata()` now works in R CMD Check thanks to @JakeVestal (#11)


# testthis 1.0.4

* Import `parse_ns_file()` from pkgload instead of devtools
* #* @testfile tag paths can now contain the script file extension `.R`
* removed deprecated `lest_this()` function
* various small bug fixes and improvements


# testthis 1.0.3

* Maintenance release to make vignette comply with new CRAN guidelines
* test (`test_this()`, `test_subdir()`, etc..) functions now save all files in 
  RStudio and reload the package before running tests.
* `lest_this()` has been deprecated. `test_this()` now behaves like 
  `lest_this()`


# testthis 1.0.2

* added `test_all()` to run tests in all subdirectories
* Use `usethis::proj_get()` instead of the `base_path` function argument for
  compatibility with usethis.


# testthis 1.0.1

* Reorganized package documentation (README, vignette, `?testthis`)
* all `pkg =` function arguments have been changed to `base_path =` for
  consistency with the **usethis** package.
* `get_test_coverage()` now recognizes tests in test subdirs


# testthis 1.0.0

* Reworked an expanded infrastructure functions, such as `use_testdata()` and
  `use_test_subdir()`. See README for details.
* `test_skeleton()` now honors the `#* @testfile` tag
* `open_test()` can now jump back and forth between source and test file, and
  does no longer automatically create test files.
* testthis now depends on the new usethis package that replaces some of the
  functionality from devtools
* Prevent creation of .rd files for unexported functions with @noRd
