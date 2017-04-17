context("get_package_functions")


test_that("get_package_functions works as expected", {


})



test_that('get_pkg_tested_functions_from_desc works', {
  #* @testing get_pkg_tested_functions_from_desc

  skip('cannot get the test to work with R CMD Check')

  tfuns <- c('fizzfun', 'buzzfun', 'foofun', '%barfun%', 'bafoon')

  tres <- with_mock(
    `testthis::get_pkg_functions` = function(...) tfuns,
    list.files                    = function(...) {
      file.path(testthat::test_path(), 'test_data', 'testthat_parse_cases.R')
    },
    get_pkg_tested_functions_from_desc(pkg = '.')
  )

  expect_identical(tres, tfuns[1:4])
})
