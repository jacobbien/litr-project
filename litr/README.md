
<!-- README.md is generated from README.Rmd. Please edit that file -->

# litr: Writing R packages via literate programming

<!-- badges: start -->
<!-- badges: end -->

The goal of `litr` is to allow you to write a complete R package in a
single R markdown document. This enables a workflow for writing R
packages that is probably very different from what you are used to.

## Why would one want to write R packages in this way?

When you try to understand the code in an R package, the logic of how
the functions relate to each other is often not obvious. While including
function documentation, vignettes, and unit tests are all best
practices, they do not convey the chain of logic in the mind of the
programmer that went into writing the different functions. It can be
difficult to know how to look through all the functions within even a
well-documented R package. The fact that the functions appear in
different files and that functions within files can be defined in
arbitrary order makes it unclear how to approach reading the code.
Furthermore, tests are stored in a different place from the functions
themselves, making the tests harder to read. This would all be resolved
if we could have a single document that goes through all code and tests
in a linear, logical fashion. Rather than try to construct such a
document after the fact, the idea of `litr` is to *make this document
the actual source code for the package*. **The R package is created
through the act of knitting this document.** If we want to modify
anything in the package, then we do so by modifying this document and
re-knitting.

The above motivation is that of [literate
programming](https://en.wikipedia.org/wiki/Literate_programming),
introduced by [Donald
Knuth](https://www-cs-faculty.stanford.edu/~knuth/), and the direct
inspiration is [fast.ai](https://www.fast.ai/about/)’s
[`nbdev`](https://nbdev.fast.ai/), which is available in Python. The
`litr` package relies heavily on a number of great tools in R,
especially [`knitr`](https://yihui.org/knitr/),
[`rmarkdown`](https://rmarkdown.rstudio.com/docs/index.html),
[`usethis`](https://usethis.r-lib.org/),
[`devtools`](https://devtools.r-lib.org/), and
[`testthat`](https://testthat.r-lib.org/). I should also note that
[Yihui Xie](https://yihui.org/en/) has [a post](https://yihui.org/rlp/)
where he demonstrates a similar idea, although his approach appears to
be more of a proof of concept.

## Getting started

You can install `litr` and get started like so:

``` r
remotes::install_github("jacobbien/litr-project", subdir = "litr")
rmarkdown::draft("my-package.Rmd", template = "make-an-r-package", package = "litr")
```

This installs `litr` and then creates an R markdown template file called
`my-package.Rmd` that demonstrates the literate programming workflow for
writing an R package. In particular, when you knit `my-package.Rmd`, it
creates a tiny example R package called `rhello` with one function and
one test function. To knit, you can either press “Knit” in RStudio or
use the following command:

``` r
litr::render("my-package.Rmd")
```
