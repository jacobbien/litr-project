# Check if package directory is the unedited output of litr::render()

Uses hash stored in a special `litr` field of DESCRIPTION file to check
that the current state of the R package directory is identical to its
state at the time that it was created by [`litr::render()`](render.md).

## Usage

``` r
check_unedited(package_dir)
```

## Arguments

- package_dir:

  Path to package
