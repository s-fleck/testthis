context("test_Test_coverage")


test_that("test_Test_coverage works as expected", {

  dat <- get_test_coverage()

})


test_that("test_Test_coverage works as expected", {

  tdat1 <- data.frame(
    fun = c('fizz', 'buzz'),
    exp = c(FALSE, FALSE),
    tested = c(TRUE, FALSE)
  )

  attr(tdat1, 'package') <- 'Test package'

  tdat2 <- data.frame(
    fun = c('fizz', 'buzz', 'foo', 'bar'),
    exp = c(FALSE, FALSE, TRUE, TRUE),
    tested = c(TRUE, FALSE, TRUE, FALSE)
  )

  attr(tdat2, 'package') <- 'Test package'

  tdat3 <- data.frame(
    fun = c('foo', 'bar'),
    exp = c(TRUE, TRUE),
    tested = c(TRUE, FALSE)
  )

  # print.Test_coverage(tdat1)
  # print.Test_coverage(tdat2)
  # print.Test_coverage(tdat3)
})


