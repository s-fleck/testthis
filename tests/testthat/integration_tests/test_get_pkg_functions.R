context("get_package_functions")




test_that('get_pkg_tested_functions_from_desc works', {
  #* @testing get_pkg_tested_functions_from_desc
  tfuns <- c('fizzfun', 'buzzfun', 'foofun', '%barfun%', 'bafoon')

  tres <- with_mock(
    `testthis::get_pkg_functions` = function(...) tfuns,
    list.files                    = function(...) {
      file.path(rprojroot::find_testthat_root_file('testdata', 'testthat_parse_cases.R'))
    },
    get_pkg_tested_functions_from_desc(pkg = '.')
  )

  expect_identical(tres, tfuns[1:4])
})
