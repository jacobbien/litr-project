# Using withpkgdown Package

This vignette describes how to use this package. You start by loading
the package.

``` r
library(withpkgdown)
```

### Saying hello

To say hello, you use the [`say_hello()`](../reference/say_hello.md)
function:

For example, to say hello to someone named Jacob, you’d do the
following:

``` r
say_hello("Jacob")
```

    ## [1] "Hello Jacob!"

### Saying hi

To say hi to someone, one would use the following:

``` r
say_hi("Jacob")
```

    ## [1] "Hi Jacob!"

### More advanced use

You can control the punctuation by using the `exclamation` argument:

``` r
say_hello("Jacob", exclamation=FALSE)
```

    ## [1] "Hello Jacob."
