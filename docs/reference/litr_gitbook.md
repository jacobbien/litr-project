# litr version of `bookdown::gitbook()`

This behaves like
[`bookdown::gitbook()`](https://pkgs.rstudio.com/bookdown/reference/gitbook.html)
with a few differences:

- It creates an R package.

- It adds hyperlinks to function definitions whenever a function is used
  elsewhere in the document.

- It does "Knuth-style" chunk referencing with hyperlinks.

## Usage

``` r
litr_gitbook(minimal_eval = FALSE, ...)
```

## Arguments

- minimal_eval:

  If `TRUE`, then only chunks with [`litr::document()`](document.md) or
  `usethis` commands will be evaluated. This can be convenient in coding
  when you just want to quickly update the R package without having to
  wait for long evaluations to occur.

- ...:

  Parameters to be passed to
  [`bookdown::gitbook()`](https://pkgs.rstudio.com/bookdown/reference/gitbook.html)
