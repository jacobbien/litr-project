# litr version of `rmarkdown::html_document()`

This behaves like
[`rmarkdown::html_document()`](https://pkgs.rstudio.com/rmarkdown/reference/html_document.html)
with a few differences:

- It creates an R package.

- It adds hyperlinks to function definitions whenever a function is used
  elsewhere in the document.

- It does "Knuth-style" chunk referencing with hyperlinks.

## Usage

``` r
litr_html_document(minimal_eval = FALSE, ...)
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
