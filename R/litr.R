#' A knitr Chunk Hook for Writing R Code and Tests
#' 
#' This chunk hook detects whether a chunk is defining a function to be included
#' in the R package (looks for the Roxygen2 comment format `#' `).  If so, then it 
#' is written to the `R/` directory.  It also looks for chunks with `testthat::`
#' in them, which are written to the `tests` directory of the R package.
#' 
#' @param before Indicates whether this is being called before or after the 
#' chunk code is executed
#' @param options Has information from the chunk
#' @param envir Environment
#' @export
send_to_package <- function(before, options, envir) {
  msg <- stringr::str_glue("# Generated from {knitr::current_input()}: do not ",
                           "edit by hand")
  if (before == FALSE || options$label == envir$setup_chunk_label) {
    # Don't do anything after the code chunk has been executed.
    # Also, don't do anything when processing this code chunk.
    return()
  }
  else if (stringr::str_detect(options$code[1], "^#' ")) {
    # starts with roxygen2, so let's assume this chunk is defining an R function
    # that belongs in the package
    non_comment <- stringr::str_subset(options$code, "^#", negate = TRUE)
    if (length(non_comment) > 0 & stringr::str_detect(non_comment[1], "<-")) {
      fname <- stringr::str_match(non_comment[1], "^(.*)\\s*<-\\s*function")[, 2]
      fname <- stringr::str_trim(fname)
      file <- file.path(
        envir$wd, envir$package_name, "R", stringr::str_glue("{fname}.R")
        )
      cat(paste(c(msg, "", options$code), collapse = "\n"), file = file)
    }
  }
  else if (any(stringr::str_detect(options$code, "testthat::"))) {
    # This chunk is inferred to be a test
    test_file <- file.path(envir$wd, envir$package_name, "tests", "testthat", "tests.R")
    if (!file.exists(test_file))
      cat(c(msg, ""), collapse = "\n", file = test_file)
    cat(
      paste(c(options$code, "", ""), collapse = "\n"),
      file = test_file,
      append = TRUE
    )
  }
  return()
}