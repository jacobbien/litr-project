# A knitr chunk hook for writing R code and tests

This chunk hook detects whether a chunk is defining a function or
dataset to be included in the R package (looks for the `roxygen2`
comment format `#' `). If so, then it is written to the `R/` directory.
It also looks for chunks that have one or more lines that start with
`test_that(` or `testthat::test_that(` (potentially with some leading
whitespace). These chunks are then written to the `tests` directory of
the R package.

## Usage

``` r
send_to_package(before, options, envir)
```

## Arguments

- before:

  Indicates whether this is being called before or after the chunk code
  is executed

- options:

  Has information from the chunk

- envir:

  Environment

## Details

When the `send_to` option is used, this chunk hook instead simply writes
the code chunk to the file specified.
