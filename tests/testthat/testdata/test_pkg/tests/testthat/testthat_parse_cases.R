context("test_get_pkg_tested_functions")

test_that("fizzfun works as expected", {

})

test_that(
  "buzzfun works as expected", {
})


test_that(
  code = 'bummer',
  desc = "even foofun is found")


test_that(
  code = expect_true(FALSE),
  "%barfun% is ok")


for (i in 1){
  test_that(paste0("blubb", i), {
    expect_true(1 == 1)
  })

  test_that("foo", {
    expect_true(1 == 2)
  })
}

