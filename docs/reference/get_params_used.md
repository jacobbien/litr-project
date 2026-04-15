# Get parameter values used in rendering

When the `params` argument of
[`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
is explicitly used, this overrides the default that appears in `input`.

## Usage

``` r
get_params_used(input, passed_params)
```

## Arguments

- input:

  The input file to be rendered (see
  [`rmarkdown::render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html))

- passed_params:

  The list of parameters that were passed to `render`.
