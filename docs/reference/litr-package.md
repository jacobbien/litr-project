# Literate Programming for Writing R Packages

Allows one to fully create an R package in a single .Rmd file. Includes
functionality and .Rmd templates for a literate programming approach to
R package development.

## See also

[`render`](render.md)

## Author

**Maintainer**: Jacob Bien <jbien@usc.edu>

Authors:

- Patrick Vossler

## Examples

``` r
if (FALSE) { # \dontrun{
# Make a file create-rhello.Rmd based on a template
litr::draft("rhello")
# Now call litr::render (or press Knit if in RStudio) to generate not just
# create-rhello.html, but also an R package called `rhello`.
litr::render("create-rhello.Rmd")
} # }
```
