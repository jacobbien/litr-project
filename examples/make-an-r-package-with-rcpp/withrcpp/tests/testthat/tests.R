# Generated from create-withrcpp.Rmd: do not edit by hand  
testthat::test_that("alternate_signs works", {
  testthat::expect_equal(alternate_signs(1:3), c(1, -2, 3))
  testthat::expect_equal(alternate_signs(c(-4, 2)), c(-4, -2))
})

