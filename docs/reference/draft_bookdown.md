# Create a new litr .Rmd document for creating an R package from `bookdown`

This template shows how to make an R package from `bookdown`. It creates
a directory called `create-[pkg_name]` in `dir`. Rendering the file
`index.Rmd` with [`litr::render()`](render.md) creates the bookdown and
package.

## Usage

``` r
draft_bookdown(pkg_name = "frombookdown", dir = ".")
```

## Arguments

- pkg_name:

  Name of package to be created.

- dir:

  (Optional) Directory where .Rmd file should be created

## See also

[`draft`](draft.md) [`draft_data`](draft_data.md)
[`draft_rcpp`](draft_rcpp.md) [`draft_extras`](draft_extras.md)
[`draft_armadillo`](draft_armadillo.md)
