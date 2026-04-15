# Create a new litr .Rmd document for creating an R package

This creates `create-[pkg_name].Rmd` that when knitted (i.e., when
passed to [`litr::render()`](render.md)) will create an R package called
`pkg_name`.

## Usage

``` r
draft(pkg_name = "rhello", dir = ".")
```

## Arguments

- pkg_name:

  Name of package to be created.

- dir:

  (Optional) Directory where .Rmd file should be created

## Details

This is the most basic R package template, with one function and one
test function.

## See also

[`draft_bookdown`](draft_bookdown.md) [`draft_data`](draft_data.md)
[`draft_rcpp`](draft_rcpp.md) [`draft_extras`](draft_extras.md)
[`draft_armadillo`](draft_armadillo.md)
