# Modify an existing output format to have `litr` behavior

This function modifies the `pre_knit()` and `post_processor()` functions
of a preexisting output format so that it will have the `litr` behavior
(meaning that an R package will be created when
[`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
is called).

## Usage

``` r
litrify_output_format(
  base_format = rmarkdown::html_document,
  minimal_eval = FALSE
)
```

## Arguments

- base_format:

  a preexisting, non-litr output format such as
  [`rmarkdown::html_document`](https://pkgs.rstudio.com/rmarkdown/reference/html_document.html)

- minimal_eval:

  If `TRUE`, then only chunks with [`litr::document()`](document.md) or
  `usethis` commands will be evaluated. This can be convenient in coding
  when you just want to quickly update the R package without having to
  wait for long evaluations to occur.
