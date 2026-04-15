# Load complete package

This is a litr wrapper to
[`devtools::load_all()`](https://devtools.r-lib.org/reference/load_all.html).
It first calls [`litr::render()`](render.md) with `minimal_eval=TRUE`,
then it calls
[`devtools::load_all()`](https://devtools.r-lib.org/reference/load_all.html)
on the generated package.

## Usage

``` r
load_all(input, output_dir = NULL, ...)
```

## Arguments

- input:

  The input file to be rendered (see
  [`rmarkdown::render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html))

- output_dir:

  By default (and in typical usage) this is NULL, meaning that no
  .html/bookdown/.pdf will result. However, when a directory is given,
  the result of the litr-knitting will be saved to this location.

- ...:

  Additional parameters to be passed to
  [`devtools::load_all()`](https://devtools.r-lib.org/reference/load_all.html)
