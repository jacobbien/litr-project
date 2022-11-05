# Generated from create-litr.Rmd: do not edit by hand

#' Run tests for `litr` itself
#' 
#' Special function for testing `litr`.  The trick is to temporarily install
#' the new version of `litr`, run the test, and then put things back how it was
#' before.
#' 
#' Typical values for `install_old` could be
#' - `function() devtools::install("[location of old version]")`
#' - `function() remotes::install_github("jacobbien/litr-project@*release", subdir = "litr")`.
#' 
#' @param install_old A function that when run will install the old version
#' @param location_of_new Path to the new package directory
#' @keywords internal
test_litr <- function(install_old, location_of_new) {
  devtools::unload(params$package_name)
  devtools::install(location_of_new)
  out <- devtools::test(location_of_new)
  install_old()
  return(out)
}
