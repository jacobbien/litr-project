# Replace a function's name with a link to its definition

A helper function for `add_function_hyperlinks` that wraps references to
a function in an anchor tag with a link to the function's definition.

## Usage

``` r
insert_hrefs(
  txt,
  function_pattern,
  where_defined,
  all_function_names,
  pkg_name,
  remove_span = FALSE
)
```

## Arguments

- txt:

  Character vector where each element is a row of the knitted HTML file.

- function_pattern:

  Regular Expression passed from `add_function_hyperlinks` that contains
  all referenced functions in the document.

- where_defined:

  Character vector that contains the name of the file in which a
  function was defined.

- all_function_names:

  Character vector of all referenced functions in the document.

- pkg_name:

  Name of the package created by litr. Taken from YAML front matter.

- remove_span:

  Boolean argument for removing span tags. Used for minimizing code
  duplication.
