# Generated from _main.Rmd: do not edit by hand  
testthat::test_that("say_hello works", {
  testthat::expect_equal(say_hello("Jacob"), "Hello Jacob!")
  testthat::expect_equal(say_hello("Jacob", exclamation = FALSE), "Hello Jacob.")
})

