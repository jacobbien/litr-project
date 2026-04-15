# R Package Templates

Predefined `litr` templates make it easy to get started writing R
packages of various kinds. The following table shows the `litr::draft*`
functions that can be used to create a `litr` .Rmd file from template.
Click on the “Generated html” and “Generated R package” cells to see
what each template produces.

| Function | Description | Rmd source file | Generated html | Generated R package |
|:---|:---|:---|:---|:---|
| [`draft()`](../reference/draft.md) | the most basic package: a function and a test | [create-rhello.Rmd](https://github.com/jacobbien/litr-project/blob/main/examples/make-an-r-package/create-rhello.Rmd) | [create-rhello.html](https://htmlpreview.github.io/?https://github.com/jacobbien/litr-project/blob/main/examples/make-an-r-package/create-rhello.html) | [rhello/](https://github.com/jacobbien/litr-project/blob/main/examples/make-an-r-package/rhello) |
| [`draft_data()`](../reference/draft_data.md) | a package with a data set in it | [create-rhasdata.Rmd](https://github.com/jacobbien/litr-project/blob/main/examples/make-an-r-package-with-data/create-rhasdata.Rmd) | [create-rhasdata.html](https://htmlpreview.github.io/?https://github.com/jacobbien/litr-project/blob/main/examples/make-an-r-package-with-data/create-rhasdata.html) | [rhasdata/](https://github.com/jacobbien/litr-project/blob/main/examples/make-an-r-package-with-data/rhasdata) |
| [`draft_rcpp()`](../reference/draft_rcpp.md) | a package using `Rcpp` | [create-withrcpp.Rmd](https://github.com/jacobbien/litr-project/blob/main/examples/make-an-r-package-with-rcpp/create-withrcpp.Rmd) | [create-withrcpp.html](https://htmlpreview.github.io/?https://github.com/jacobbien/litr-project/blob/main/examples/make-an-r-package-with-rcpp/create-withrcpp.html) | [withrcpp/](https://github.com/jacobbien/litr-project/blob/main/examples/make-an-r-package-with-rcpp/withrcpp) |
| [`draft_extras()`](../reference/draft_extras.md) | a package with a README, vignette(s), a pkgdown site, and a hex sticker | [create-withpkgdown.Rmd](https://github.com/jacobbien/litr-project/blob/main/examples/make-an-r-package-with-extras/create-withpkgdown.Rmd) | [create-withpkgdown.html](https://htmlpreview.github.io/?https://github.com/jacobbien/litr-project/blob/main/examples/make-an-r-package-with-extras/create-withpkgdown.html) | [withpkgdown/](https://github.com/jacobbien/litr-project/blob/main/examples/make-an-r-package-with-extras/withpkgdown) |
| [`draft_bookdown()`](../reference/draft_bookdown.md) | a package defined in a bookdown | [index.Rmd](https://github.com/jacobbien/litr-project/blob/main/examples/make-an-r-package-from-bookdown/index.Rmd), [1description.Rmd](https://github.com/jacobbien/litr-project/blob/main/examples/make-an-r-package-from-bookdown/1description.Rmd), …, [4end.Rmd](https://github.com/jacobbien/litr-project/blob/main/examples/make-an-r-package-from-bookdown/4end.Rmd) | [\_book/index.html](https://htmlpreview.github.io/?https://github.com/jacobbien/litr-project/blob/main/examples/make-an-r-package-from-bookdown/_book/index.html) | [frombookdown/](https://github.com/jacobbien/litr-project/blob/main/examples/make-an-r-package-from-bookdown/frombookdown) |
| [`draft_armadillo()`](../reference/draft_armadillo.md) | a package using `RcppArmadillo` | [create-witharmadillo.Rmd](https://github.com/jacobbien/litr-project/blob/main/examples/make-an-r-package-with-armadillo/create-witharmadillo.Rmd) | [create-witharmadillo.html](https://htmlpreview.github.io/?https://github.com/jacobbien/litr-project/blob/main/examples/make-an-r-package-with-armadillo/create-witharmadillo.html) | [witharmadillo/](https://github.com/jacobbien/litr-project/blob/main/examples/make-an-r-package-with-armadillo/witharmadillo) |

Once you’ve chosen a template, you can get started by calling the
corresponding `litr::draft*` function from the table above.

For example, for the most basic template, you would do the following:

``` r
litr::draft("rhello")
litr::render("create-rhello.Rmd")
```

And to make an R package that uses `Rcpp`, you would start with the
following:

``` r
litr::draft_rcpp("withrcpp")
litr::render("create-withrcpp.Rmd")
```
