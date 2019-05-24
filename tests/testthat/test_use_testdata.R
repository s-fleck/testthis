context("use_testdata")



test_that("test_parse_testthis_comments works as expected", {
  x <- read_testdata("read_testdata_testfile.rds")
  expect_identical(x, "185")
})
