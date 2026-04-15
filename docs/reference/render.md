# Render R markdown file

Wrapper to
[`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
that produces an R package as output in addition to the standard output
document. It does some post-processing on the .html file when that is
the output. In particular, when an .html file is among the outputs, it
adds hyperlinks to functions defined within the file to make it easier
for someone reading the code to see where different functions are
defined.

## Usage

``` r
render(input, minimal_eval, fresh_session = TRUE, ...)
```

## Arguments

- input:

  The input file to be rendered (see
  [`rmarkdown::render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html))

- minimal_eval:

  If `TRUE`, then only chunks with [`litr::document()`](document.md) or
  `usethis` commands will be evaluated. This can be convenient in coding
  when you just want to quickly update the R package without having to
  wait for long evaluations to occur.

- fresh_session:

  Whether to call
  [`rmarkdown::render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
  from a fresh R session. By default TRUE, so that it matches the
  behavior of pressing "Knitr" in RStudio. However, for debugging it can
  be useful to set this to FALSE so that functions like
  [`debug()`](https://rdrr.io/r/base/debug.html) and
  [`browser()`](https://rdrr.io/r/base/browser.html) will work.

- ...:

  Additional parameters to pass to
  [`rmarkdown::render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
