# Add litr hash to DESCRIPTION file if error encountered

This creates a function that calls the passed function within the
context of a try-catch. If an error is encountered, the litr hash is
still added to the DESCRIPTION file so that future calls to
[`litr::render()`](render.md) will recognize that it can safely
overwrite the package directory (i.e., no manual editing occurred).

## Usage

``` r
with_cleanup(fun, package_dir)
```

## Arguments

- fun:

  function being called

- package_dir:

  directory where package is being written to

- ...:

  arguments to be passed to `fun`
