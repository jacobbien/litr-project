# Create a new litr .Rmd document for creating an R package with a data set

This creates `create-[pkg_name].Rmd` that when knitted (i.e., when
passed to [`litr::render()`](render.md)) will create an R package called
`pkg_name`. This template shows how to make an R package with a data set
in it.

## Usage

``` r
draft_data(pkg_name = "rhasdata", dir = ".")
```

## Arguments

- pkg_name:

  Name of package to be created.

- dir:

  (Optional) Directory where .Rmd file should be created

## See also

[`draft`](draft.md) [`draft_bookdown`](draft_bookdown.md)
[`draft_rcpp`](draft_rcpp.md) [`draft_extras`](draft_extras.md)
[`draft_armadillo`](draft_armadillo.md)
