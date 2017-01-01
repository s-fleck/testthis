context("test_Taglist")

tfile <-  system.file('tests', 'testthat', 'test_data', 'testthis_tags.R',
                      package = 'testthis')

test_that("test_parse_testthis_comments works as expected", {
  #* @testing detect_testthis_comments ------
  t1 <- '#* @testing  testthis_tokenize'
  t2 <- '#* @testfile testthis_tokenize -----'
  t3 <- '#* @testfile testthis_tokenize ----- \n
  #  @blubb \n
  #  blahblah'

  expect_true(detect_testthis_comments(t1))
  expect_true(detect_testthis_comments(t2))
  expect_true(detect_testthis_comments(t3))

  #* @testing extract_testthis_comments
  tdat  <- extract_testthis_comments(infile = tfile)

  expect_identical('#* @testfile testthis_tags2', tdat[[2]])
  expect_identical('  #* @testing detect_testthis_comments ------', tdat[[3]])
  expect_true(grepl('@testing extract_testthis_comments', tdat[[4]]))
  expect_true(grepl('@testing testthis_tokenizer', tdat[[5]]))


  #* @testing testthis_tokenizer
  expect_identical(testthis_tokenizer(tdat[[2]]), c("@testfile", "testthis_tags2"))
  expect_identical(testthis_tokenizer(tdat[[3]]), c("@testing", "detect_testthis_comments"))
  expect_identical(testthis_tokenizer(tdat[[4]]), c("@testing", "extract_testthis_comments"))
  expect_identical(testthis_tokenizer(tdat[[5]]), c("@testing", "testthis_tokenizer"))
})


test_that("test_parse_testthis_comments works as expected", {
  expect_identical(
    get_taglist(tfile)[3:5],
    list(c("@testing", "detect_testthis_comments"),
         c("@testing", "extract_testthis_comments"),
         c("@testing", "testthis_tokenizer"))
  )
})

test_that("extracting testthis tags works as expected", {
  #* @testing get_taglist

  eres <-
    list(
      c("@testing", "detect_testthis_comments"),
      c("@testing",
        "extract_testthis_comments"),
      c("@testing", "testthis_tokenizer"),
      c("@testing", "get_taglist"),
      c("@testing", "%foofun%")
    )

  expect_identical(get_taglist(tfile)[3:7], eres)
})


test_that("get_tag works as expected", {

  tlist <- get_taglist(tfile)

  expect_identical(get_tag(tlist, 'testfile'),
                   c('testthis_tags', 'testthis_tags2'))

  expect_identical(get_tag(tlist, 'testing'),
                   c("detect_testthis_comments", "extract_testthis_comments",
                     "testthis_tokenizer", "get_taglist", "%foofun%"))

})

