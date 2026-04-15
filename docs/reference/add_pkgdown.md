# Add a pkgdown site

This function creates a website for your package. You can see it locally
by opening `docs/index.html` in your package. To get it online you can
copy the `docs` directory to your website's server.

## Usage

``` r
add_pkgdown(config_path = NULL)
```

## Arguments

- config_path:

  The \_pkgdown.yml file that lives somewhere outside of your package.
  If NULL, then a basic default will be used.

## Details

Be sure that in the generating .Rmd file this is called *after*
[`litr::document()`](document.md) has been called. To customize the
site, you may pass a customized `_pkgdown.yml` file as described in
[this `pkgdown`
vignette](https://pkgdown.r-lib.org/articles/customise.html).
