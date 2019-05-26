context("Test_coverage")


test_that("get_pkg_tested_functions_from_desc", {

  list_test_files_mock <- function(...) {
    paste0("/blah/blubb/tests/testthat/", c("test_foo", "test_bar"))
  }

  extract_test_that_desc_mock <- function(...){
    list(
      c("foofun1", "testing foofun2"),
      c("barfun1", "barfun2", "foofun1")
    ) %>%
      setNames(list_test_files_mock())
  }

  get_pkg_functions_mock <- function(...){
    c("foofun1", "foofun2", "barfun1", "barfun2")
  }


  tres <- with_mock(
    get_pkg_tested_functions_from_desc(),
    list_test_files = list_test_files_mock,
    extract_test_that_desc = extract_test_that_desc_mock,
    get_pkg_functions = get_pkg_functions_mock
  )


  expect_identical(
    tres,
    list(
      foofun1 =
        c("/blah/blubb/tests/testthat/test_foo", "/blah/blubb/tests/testthat/test_bar"),
      foofun2 = "/blah/blubb/tests/testthat/test_foo",
      barfun1 = "/blah/blubb/tests/testthat/test_bar",
      barfun2 = "/blah/blubb/tests/testthat/test_bar")
  )
})




test_that("test_coverage works", {
  expect_silent(test_coverage())
  expect_silent(test_coverage(from_tags = FALSE))
  expect_silent(test_coverage(from_desc = FALSE))
})
