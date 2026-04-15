# Add README to package

This function takes a README.Rmd file, copies it into the package, and
then renders it to a README.md file. It also adds these two files to the
.Rbuildignore.

## Usage

``` r
add_readme(rmd_file)
```

## Arguments

- rmd_file:

  The path to a .Rmd file.
