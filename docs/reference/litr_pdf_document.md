# litr version of `rmarkdown::pdf_document()`

This behaves exactly like
[`rmarkdown::pdf_document()`](https://pkgs.rstudio.com/rmarkdown/reference/pdf_document.html)
except it creates an R package.

## Usage

``` r
litr_pdf_document(minimal_eval = FALSE, ...)
```

## Arguments

- minimal_eval:

  If `TRUE`, then only chunks with [`litr::document()`](document.md) or
  `usethis` commands will be evaluated. This can be convenient in coding
  when you just want to quickly update the R package without having to
  wait for long evaluations to occur.

- ...:

  Parameters to be passed to
  [`rmarkdown::pdf_document()`](https://pkgs.rstudio.com/rmarkdown/reference/pdf_document.html)
