# Package index

## Primary functions

These are the functions you’ll use the most.

- [`draft()`](draft.md) : Create a new litr .Rmd document for creating
  an R package
- [`render()`](render.md) : Render R markdown file
- [`document()`](document.md) : Use roxygen to document a package from
  within a Rmd file
- [`load_all()`](load_all.md) : Load complete package

## Working from template

These are functions for quickly creating different kinds of R packages.

- [`draft()`](draft.md) : Create a new litr .Rmd document for creating
  an R package

- [`draft_bookdown()`](draft_bookdown.md) :

  Create a new litr .Rmd document for creating an R package from
  `bookdown`

- [`draft_data()`](draft_data.md) : Create a new litr .Rmd document for
  creating an R package with a data set

- [`draft_rcpp()`](draft_rcpp.md) :

  Create a new litr .Rmd document for creating an R package that uses
  `Rcpp`

- [`draft_extras()`](draft_extras.md) : Create a new litr .Rmd document
  for creating an R package with extras

- [`draft_armadillo()`](draft_armadillo.md) :

  Create a new litr .Rmd document for creating an R package that uses
  `RcppArmadillo`

## Functions for adding “extras” to your package

These functions can help you add a README, vignettes, a pkgdown site,
and a hex sticker to your package.

- [`add_readme()`](add_readme.md) : Add README to package
- [`add_vignettes()`](add_vignettes.md) : Add one or more vignettes to
  package
- [`add_pkgdown()`](add_pkgdown.md) : Add a pkgdown site
- [`add_hex_sticker()`](add_hex_sticker.md) : Add a hex sticker to
  package

## Custom output formats

These are the functions for producing different output formats.

- [`litr_html_document()`](litr_html_document.md) :

  litr version of
  [`rmarkdown::html_document()`](https://pkgs.rstudio.com/rmarkdown/reference/html_document.html)

- [`litr_pdf_document()`](litr_pdf_document.md) :

  litr version of
  [`rmarkdown::pdf_document()`](https://pkgs.rstudio.com/rmarkdown/reference/pdf_document.html)

- [`litr_gitbook()`](litr_gitbook.md) :

  litr version of
  [`bookdown::gitbook()`](https://pkgs.rstudio.com/bookdown/reference/gitbook.html)

- [`litrify_output_format()`](litrify_output_format.md) :

  Modify an existing output format to have `litr` behavior
