# Add hyperlinks to function definitions

Finds functions that are defined in the html file(s) by looking for text
of the form `foo <- function(` and then wraps `foo` in a `span` tag with
`id="foo"` and then whenever `foo` is found it wraps a
`a href="file.html#foo"` tag so that it will be a hyperlink to `foo`'s
definition.

## Usage

``` r
add_function_hyperlinks(html_files, pkg_name)
```

## Arguments

- html_files:

  Character vector of file names of html files that were created from
  Rmd files

- pkg_name:

  Name of the package created by litr. Taken from YAML front matter
