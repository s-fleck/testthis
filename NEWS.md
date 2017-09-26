# testthis 1.0.0

* Reworked an expanded infrastructure functions, such as `use_testdata()` and
  `use_test_subdir()`. See readme for details.
* `test_skeleton()` now honours the `#* @testfile` tag
* `open_test()` can now jump back and forth between source and test file, and
  does no longer automatically create test files.
* testthis now depends on the new usethis package that replaces some of the
  functionality from devtools
* Prevent creation of .rd files for unexported functions with @noRd
