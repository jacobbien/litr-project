# Use roxygen to document a package from within a Rmd file

This is a wrapper for the
[`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
function, which in turn is a wrapper for the
[`roxygen2::roxygenize()`](https://roxygen2.r-lib.org/reference/roxygenize.html)
function. It is written assuming that it is being called from within a
generating Rmd file. The purpose for `litr` having this wrapper is
two-fold. First, it ensures that the first line in the outputted `Rd`
files should not say "Please edit documentation in R/file.R" but instead
should refer to the Rmd file that generates everything. Second, in the
case that Rcpp is being used, it makes some adjustments to ensure that
the compiling of the C++ code should be successful.

## Usage

``` r
document(...)
```

## Arguments

- ...:

  Arguments to be passed to
  [`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
