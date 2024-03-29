
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Creating `litr`

<!-- badges: start -->
<!-- badges: end -->

**Note:** If you are looking to use the `litr` package, please visit the
`litr` website [here](https://jacobbien.github.io/litr-project/), where
you’ll be able to learn about the package. If you are interested in how
`litr` is made, you are in the right place.

## Background

The `litr` R package allows one to write R packages using literate
programming. The developers of `litr` are believers in literate
programming and so, quite naturally, want to use literate programming to
develop `litr`. For obvious reasons, we couldn’t use `litr` before it
existed. So we wrote the initial viable version in a standard,
non-literate way and released this as
[v0.0.1](https://github.com/jacobbien/litr-project/releases/tag/v0.0.1).
We can think of this as the “base case.” This is the last version that
is written in the traditional way. Every subsequent version of `litr`
will be generated using the previous version of `litr`. In particular,
v0.0.2 of `litr` is functionally equivalent to v0.0.1, but the package
is defined in a generating .Rmd file; a call to v0.0.1’s
`litr::render()` outputs the `litr` package v0.0.2. This all might sound
complicated, but one can think of this as how developers of an operating
system probably write their code on a computer that is running the
previous stable version of the operating system.

## How to generate a new version of `litr` using the previous release

To create a new version of `litr`, we first install the latest release,
then make any desired changes to `create-litr.Rmd` and then use the
installed version’s `litr::render()` to create the new version:

``` r
remotes::install_github("jacobbien/litr-project@*release", subdir = "litr")
litr::render("create-litr/index.Rmd")
```

In the above code, `@*release` stands for the latest release. For
example, at the time of creating version `0.0.2`, this would be
`v0.0.1`.

This will generate the new version of `litr` along with [this
bookdown](https://jacobbien.github.io/litr-project/create/). From there,
you can build/install as you would for any other package. For checking
the package, use

``` r
devtools::check("litr", document = FALSE)
```

The `document = FALSE` prevents `devtools` from running its version of
`document()` internally, which would overwrite the modifications that
`litr::document()` has made.

For more notes on contributing to `litr`, please see
[CONTRIBUTING.md](CONTRIBUTING.md).
