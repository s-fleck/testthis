context("test_Taglist")
#* @skip



test_that("test_parse_testthis_comments works as expected", {
  tfile <-  file.path(
    testthat::test_path(), "testdata", "test_pkg", "tests", "testthat", "testthis_tags.R"
  )

  #* @testing detect_testthis_comments ------
  t1 <- '#* @testing  testthis_tokenize'
  t2 <- '#* @testfile testthis_tokenize -----'
  t3 <- '#* @testfile testthis_tokenize ----- \n
  #  @blubb \n
  #  blahblah'
  t4 <- '#* @skip'

  expect_true(detect_testthis_comments(t1))
  expect_true(detect_testthis_comments(t2))
  expect_true(detect_testthis_comments(t3))
  expect_true(detect_testthis_comments(t4))

  #* @testing extract_testthis_comments
  tdat  <- extract_testthis_comments(infile = tfile)

  expect_identical('#* @skip', tdat[[1]])
  expect_identical('#* @testfile testthis_tags2', tdat[[3]])
  expect_identical('  #* @testing detect_testthis_comments ------', tdat[[4]])
  expect_true(grepl('@testing extract_testthis_comments', tdat[[5]]))
  expect_true(grepl('@testing testthis_tokenizer', tdat[[6]]))


  #* @testing testthis_tokenizer
  expect_identical(testthis_tokenizer(tdat[[1]]), c("@skip"))
  expect_identical(testthis_tokenizer(tdat[[3]]), c("@testfile", "testthis_tags2"))
  expect_identical(testthis_tokenizer(tdat[[4]]), c("@testing", "detect_testthis_comments"))
  expect_identical(testthis_tokenizer(tdat[[5]]), c("@testing", "extract_testthis_comments"))
  expect_identical(testthis_tokenizer(tdat[[6]]), c("@testing", "testthis_tokenizer"))
})


test_that("extracting testthis tags works as expected", {
  tfile <-  file.path(
    testthat::test_path(), "testdata", "test_pkg", "tests", "testthat", "testthis_tags.R"
  )

  tdat <- get_taglist(tfile)

  expect_s3_class(tdat, 'Taglist')

  expect_identical(
    names(tdat),
    c("skip", "testfile", "testing")
  )

  expect_identical(tdat$skip, TRUE)
  expect_identical(tdat$testfile, c("testthis_tags", "testthis_tags2"))
  expect_identical(
    tdat$testing,
    c("%foofun%", "detect_testthis_comments", "extract_testthis_comments",
      "get_taglist", "testthis_tokenizer")
  )
})


test_that("get_tag works as expected", {
  tfile <-  file.path(
    testthat::test_path(), "testdata", "test_pkg", "tests", "testthat", "testthis_tags.R"
  )

  tlist <- get_taglist(tfile)

  expect_identical(get_tag(tlist, 'testfile'),
                   c('testthis_tags', 'testthis_tags2'))

  expect_identical(
    get_tag(tlist, 'testing'),
    c("%foofun%", "detect_testthis_comments", "extract_testthis_comments",
      "get_taglist", "testthis_tokenizer")
  )
})

