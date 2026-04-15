# Add hyperlinks to embedded chunks

Finds chunks that are referenced in the html file(s) by looking for
comments of the form `###"foo"###` and then wraps `foo` in a `span` tag
with `id="foo"` and then whenever the chunk label `<<foo>>` is found it
wraps it in a `a href="file.html#foo"` tag so that it will be a
hyperlink to `foo`'s definition.

## Usage

``` r
add_chunk_label_hyperlinks(
  html_files,
  reference_start = "&lt;&lt;",
  reference_end = "&gt;&gt;"
)
```

## Arguments

- html_files:

  Character vector of file names of html files that were created from
  Rmd files

- reference_start:

  The delimiter used to indicate the start of a chunk label

- reference_end:

  The delimiter used to indicate the end of a chunk label
