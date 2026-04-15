# Code for setup chunk

- Creates directory where package will be. (Deletes what is currently
  there as long as it appears to have been created by litr and does not
  have any subsequent manual edits.)

- Sets the root directory to this directory

- Sets up the main chunk hook
  [`litr::send_to_package()`](send_to_package.md) that sends code to the
  R package directory.

- In the case that `minimal_eval=TRUE`, sets up an options hook for
  `eval` so chunks are only evaluated if there is a `usethis` or
  [`litr::document()`](document.md) command

- Deactivates an internal function of the `usethis` package

- Redefines the document output hook to handle chunk references
  differently

- Sets up a [custom language
  engine](https://bookdown.org/yihui/rmarkdown-cookbook/custom-engine.html)
  called `package_doc` that creates a package documentation file and
  then inserts whatever the user puts in the chunk.

## Usage

``` r
setup(package_dir, minimal_eval)
```

## Arguments

- package_dir:

  Directory where R package will be created

- minimal_eval:

  If `TRUE`, then only chunks with [`litr::document()`](document.md) or
  `usethis` commands will be evaluated. This can be convenient in coding
  when you just want to quickly update the R package without having to
  wait for long evaluations to occur.

## Details

Returns the original state of the knitr objects that have been modified
in setup. This allows us to return things to the previous state after we
are finished. This is relevant in the case where litr-knitting occurs in
the current session and we don't want to leave things in a permanently
modified state.
