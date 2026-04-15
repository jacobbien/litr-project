# Run tests for `litr` itself

Special function for testing `litr`. The trick is to temporarily install
the new version of `litr`, run the test, and then put things back how it
was before.

## Usage

``` r
test_litr(install_old, location_of_new)
```

## Arguments

- install_old:

  A function that when run will install the old version

- location_of_new:

  Path to the new package directory

## Details

Typical values for `install_old` could be

- `function() devtools::install("[location of old version]")`

- `function() remotes::install_github("jacobbien/litr-project@*release", subdir = "litr")`.
