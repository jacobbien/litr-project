# Generated from create-rF2C.Rmd: do not edit by hand  
testthat::test_that("F2C works", {
  testthat::expect_equal(F2C(90), 32.222222)
  testthat::expect_error(F2C("90°F"))
})

