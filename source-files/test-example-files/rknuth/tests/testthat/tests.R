# Generated from create-rknuth.Rmd: do not edit by hand  
testthat::test_that('add_five() works', {
  testthat::expect_equal(add_five(2), 7)
})

testthat::test_that('add_seven_and_double() works', {
  testthat::expect_equal(add_seven_and_double(1), 2 * (1 + 7))
})

testthat::test_that('several calls to function works', {
  add_five(1)
  add_five(-2)
  add_five(10)
  testthat::expect_equal(add_seven_and_double(-1), 2 * (-1 + 7))
})

