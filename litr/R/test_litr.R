# Generated from create-litr.Rmd: do not edit by hand

#' Run tests for `litr` itself
#' 
#' Special function for testing `litr`.  The trick is to temporarily install
#' the new version of `litr`, run the test, and then put things back how it was
#' before.
#' 
#' Typical values for `install_old` could be
#' - `function() devtools::install("[location of old version]")`
#' - `function() remotes::install_github("jacobbien/litr-project@*release", subdir = "litr")`
#' 
#' @param install_old A function that when run will install the old version
#' @param location_of_new Path to the new package directory
#' @export
test_litr <- function(install_old, location_of_new) {
  devtools::install(location_of_new)
  # need this to handle devtools::install's switch to using pkgload::unregister
  # instead of pkgload::unload devtools@v2.4.4
  pkgload::unload("litr")
  raw_test_results <- devtools::test(location_of_new, reporter = testthat::ListReporter)
  
  
  
  test_results_parsed <- raw_test_results |>
  dplyr::as_tibble() |>
  dplyr::rename(Test = test) |>
  dplyr::group_by(file, context, Test) |>
  dplyr::summarise(NumTests = dplyr::first(nb),
            Passed   = sum(passed),
            Failed   = sum(failed),
            Warnings = sum(warning),
            Errors   = sum(as.numeric(error)),
            Skipped  = sum(as.numeric(skipped)),
            .groups = "drop")
  # out <- test_results_parsed %>% dplyr::summarise_if(is.numeric, sum)
  test_results_nested <- test_results_parsed |>
  dplyr::nest_by(file, context, .key = "Results") |>
  dplyr::nest_by(file, .key = "Categories")
  
  install_old()
  
  return(test_results_nested)
}
