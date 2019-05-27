context("get_test_index")




setup({
  tenv <- parent.env(environment())
  proj_old <- tryCatch(usethis::proj_get(), error = function(e) NULL)
  assign("proj_old", proj_old, tenv)
  assign("td", file.path(tempdir(), "testthis"), tenv)
  assign("proj_test", file.path(td, "test_pkg"), tenv)

  dir <- find_testdata("test_pkg", must_exist = TRUE)
  fs::dir_copy(dir, proj_test)
  usethis::proj_set(proj_test)
})




teardown({
  usethis::proj_set(proj_old)
  unlink(td, recursive = TRUE)
})




test_that("extract_testthat_parse_data works as expected", {
  # works with text
  txt <- "test_that(\n\n'blubb')"
  x <- extract_testthat_parse_data(txt)
  expect_identical(x$text, txt)

  # works with expressions
  txt <- "test_that(x = y, desc = 'blubb')"
  x <- extract_testthat_parse_data(parse(text = txt, keep.source = TRUE))
  expect_identical(x$text, txt)

  # works with nested expressions
  txt <- "for (i in 1:10) {test_that(paste0('blubb', i)); test_that('foo')}"
  x <- extract_testthat_parse_data(txt)
  expect_identical(x$text, c("test_that(paste0('blubb', i))", "test_that('foo')"))
})




test_that("extract_testthat_desc works as expected", {
  # works with text
  txt <- "test_that(\n\n'blubb')"
  x <- extract_testthat_desc(txt)
  expect_identical(x, "blubb")

  # works with expressions
  txt <- "test_that(x = y, desc = 'blubb', {  x <- y \n z = 3})"
  x <- extract_testthat_desc(parse(text = txt, keep.source = TRUE))
  expect_identical(x, "blubb")

  # works with nested expressions
  txt <- "test_that(paste0('blubb', i), LETTERS)"
  x <- extract_testthat_desc(txt)
  expect_match(x, c("^paste0.*(.*blubb.*, i.*)$"))

  expect_error({
    txt <- "for (i in 1:10) {test_that(paste0('blubb', i), letters); test_that('foo')}"
    x <- extract_testthat_desc(txt)
  })
})




test_that("collect_testthat_source_info works as expected", {
  src <-
    find_testdata("test_pkg", "tests", "testthat", "testthat_parse_cases.R")

  x <- collect_testthat_source_info(src)

  expect_equal(
    x$desc,
    c("fizzfun works as expected",
      "buzzfun works as expected",
      "even foofun is found",
      "%barfun% is ok",
      "paste0(\"blubb\", i)",
      "foo"
    )
  )

  expect_identical(x$line1, c(3L, 7L, 12L, 17L, 23L, 27L))
})
