context("test_parse_testthis_comments")

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

  expect_identical('  #* @testing detect_testthis_comments ------', tdat[[1]])
  expect_true(grepl('@testing extract_testthis_comments', tdat[[2]]))
  expect_true(grepl('@testing testthis_tokenizer', tdat[[3]]))


  #* @testing testthis_tokenizer
  expect_identical(testthis_tokenizer(tdat[[1]]), c("@testing", "detect_testthis_comments"))
  expect_identical(testthis_tokenizer(tdat[[2]]), c("@testing", "extract_testthis_comments"))
  expect_identical(testthis_tokenizer(tdat[[3]]), c("@testing", "testthis_tokenizer"))
})


test_that("test_parse_testthis_comments works as expected", {
  expect_identical(
    extract_testthis_tags(tfile)[1:3],
    list(c("@testing", "detect_testthis_comments"),
         c("@testing", "extract_testthis_comments"),
         c("@testing", "testthis_tokenizer"))
  )
})

test_that("extracting testthis tags works as expected", {
  #* @testing extract_testthis_tags

  eres <-
    list(
      c("@testing", "detect_testthis_comments"),
      c("@testing",
        "extract_testthis_comments"),
      c("@testing", "testthis_tokenizer"),
      c("@testing", "extract_testthis_tags"),
      c("@testing", "%foofun%")
    )

  expect_identical(extract_testthis_tags(tfile), eres)

})

