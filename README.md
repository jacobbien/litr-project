
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Creating `litr`

<!-- badges: start -->
<!-- badges: end -->

**Note:** If you are looking to use the `litr` package, please go to the
`litr` directory [here](litr), where you’ll be able to learn about the
package. If you are interested in how `litr` is made, you are in the
right place.

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

## How to generate `litr` version 0.0.2 using version 0.0.1

To create `litr` version 0.0.2, we first install `litr` version 0.0.1,
then download [`create-litr.Rmd` as it was at release
v0.0.2](https://github.com/jacobbien/litr-project/blob/66cc7c9286e43cb3f1438f4aa70c504a99782f2d/create-litr.Rmd)
and then run the following command in R:

``` r
remotes::install_github("jacobbien/litr-project@*release", subdir = "litr")
litr::render("create-litr.Rmd")
```

In the above code, `@*release` stands for the latest release, which at
the time of creating version `0.0.2` was `v0.0.1`.

This will generate the new version of `litr`. From there, you can
build/install as you would for any other package.
