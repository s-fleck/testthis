context("test_parse_testthis_comments")
#* @skip

tfile <-  system.file('tests', 'testthat', 'test_parse_testthis_comments.R', package = 'testthis')

#* @testfile testthis_tags
#* @testfile testthis_tags2

test_that("test_parse_testthis_comments works as expected", {
  #* @testing detect_testthis_comments ------
  t1 <- '#* @testing  testthis_tokenize'
  t2 <- '#* @testfile testthis_tokenize -----'
  t3 <- '#* @testfile testthis_tokenize ----- \n
         #  @blubb \n
         #  blahblah'

  #* @testing extract_testthis_comments
  tdat  <- extract_testthis_comments(infile = tfile)

  #* @testing testthis_tokenizer
})


test_that("test_parse_testthis_comments works as expected", {

})

test_that("extracting testthis tags works as expected", {
  #* @testing get_taglist
  #* @testing %foofun%
})
