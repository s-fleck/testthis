context("test_get_tested_functions")

test_that("get_tested_functions_from_tags works as expected", {


})

test_that('get_tested_functions_from_desc works', {
  tfuns <- c('fizzfun', 'buzzfun', 'foofun', '%barfun%', 'bafoon')

  tres <- with_mock(
    get_all_functions = function(...) tfuns,
    list.files            = function(...) {
      system.file('tests', 'testthat', 'test_data', 'testthat_parse_cases.R',
                  package = 'testthis')
      },
    get_tested_functions_from_desc(pkg = '.')
  )

  expect_identical(tres, tfuns[1:4])
})

