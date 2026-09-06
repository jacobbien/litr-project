# Generated from create-rhasdata.Rmd: do not edit by hand

set.seed(123)
n <- 100
x <- rnorm(n)
y <- 2 * x + 0.3 * rnorm(n)
mydata <- data.frame(x = x, y = y)
