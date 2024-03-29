proj <- rprojroot::find_package_root_file()

test_that('get_pkg_tested_functions_from_desc works', {
  skip("Update to user mockery")
  #* @testing get_pkg_tested_functions_from_desc
  tpkg <- file.path(rprojroot::find_testthat_root_file("testdata", "test_pkg", "tests", "testthat"))
  usethis::proj_set(tpkg)

  tfuns <- c('fizzfun', 'buzzfun', 'foofun', '%barfun%', 'bafoon')


  tres <- with_mock(
    `testthis::get_pkg_functions` = function(...) tfuns,
    get_pkg_tested_functions_from_desc()
  )

  expect_identical(names(tres[sapply(tres, length) > 0]), tfuns[1:4])

  usethis::proj_set(proj)
})
