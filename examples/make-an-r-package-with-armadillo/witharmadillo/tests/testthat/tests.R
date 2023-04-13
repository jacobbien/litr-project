# Generated from create-witharmadillo.Rmd: do not edit by hand  
testthat::test_that("my_chol works", {
  x <- matrix(0, 3, 3)
  x[upper.tri(x, diag = TRUE)] <- 1:6
  xchol <- my_chol(crossprod(x))
  testthat::expect_equal(crossprod(xchol), crossprod(x))
  testthat::expect_true(all(xchol[lower.tri(xchol)] == 0))
})

