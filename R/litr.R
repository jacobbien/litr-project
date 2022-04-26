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
  msg <- do_not_edit_message(knitr::current_input(), type = "R")
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
        envir$package_dir, "R", stringr::str_glue("{fname}.R")
        )
      cat(paste(c(msg, "", options$code), collapse = "\n"), file = file)
    }
  }
  else if (any(stringr::str_detect(options$code, "testthat::"))) {
    # This chunk is inferred to be a test
    test_file <- file.path(envir$package_dir, "tests", "testthat", "tests.R")
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

#' Generate do-not-edit message to put at top of file
#' 
#' @param rmd_file Name of the Rmd file to mention
#' @param type Whether this is a R/ file or a man/ file
do_not_edit_message <- function(rmd_file, type = c("R", "man")) {
  if (type[1] == "R")
    return(stringr::str_glue("# Generated from {rmd_file}: do not edit by hand"))
  else if (type[1] == "man")
    return(stringr::str_glue("% Please edit documentation in {rmd_file}."))
  else
    stop("type must be either 'R' or 'man'.")
}

#' Use roxygen to document a package
#' 
#' This is a wrapper for the `devtools::document()` function, which in turn is a
#' wrapper for the `roxygen2::roxygenize()` function.  The purpose for `litr` 
#' having this wrapper is to make one modification.  In particular, the line
#' in the outputted `Rd` files should not say "Please edit documentation in 
#' R/file.R" but instead should refer to the Rmd file that generates everything. 
#' 
#' @param ... Arguments to be passed to `devtools::document()`
#' @export
document <- function(...) {
  devtools::document(...)
  # remove the line of the following form in each man/*.Rd file:
  pattern <- "% Please edit documentation in .*$"
  msg <- do_not_edit_message(knitr::current_input(), type = "man")
  for (fname in fs::dir_ls("man")) {
    txt <- stringr::str_replace(readLines(fname), pattern, msg)
    cat(paste(txt, collapse = "\n"), file = fname)
  }
}

#' Add hyperlinks to function definitions
#' 
#' Finds functions that are defined in the html file by looking for text of the 
#' form `foo <- function(` and then wraps `foo` in a `span` tag with `id="foo"` 
#' and then whenever `foo` is found it wraps a `a href="#foo"` tag so that it be
#' a hyperlink to `foo`'s definition.
#' 
#' @param html_file File name of html file that was created from Rmd file
#' @param output_file File name to output to. Default: `html_file`
#' @export
add_function_hyperlinks <- function(html_file, output_file = html_file) {
  txt <- readLines(html_file)
  start_line <- which(txt == "<body>")
  pattern <- "([a-zA-Z0-9_.]+)(\\s*&lt;-\\s*function)"
  # find functions that are defined in this file:
  function_names <- character(0)
  for (i in seq(start_line + 1, length(txt))) {
    fn_name <- stringr::str_match(txt[i], pattern)[, 2]
    if(is.na(fn_name)) next
    # a function was defined in this line, so put a span around it
    txt[i] <- stringr::str_replace(
      txt[i],
      pattern,
      stringr::str_glue("<span id='{fn_name}'>\\1</span>\\2")
      )
    # and keep track of it for later:
    function_names <- c(function_names, fn_name)
  }
  
  # whenever one of these named functions is named, link to its definition
  txt <- stringr::str_replace_all(
    txt,
    paste0(function_names, "\\(", collapse = "|"),
    function(x) {
      fn_name <- stringr::str_remove(x, "\\(")
      stringr::str_glue("<a href='#{fn_name}'>{fn_name}</a>(")
    }
  )
  writeLines(txt, con = output_file)
}

#' Render R markdown file
#' 
#' Wrapper to `rmarkdown::render` that does some post-processing on the html 
#' file when that is the output.  In particular, when an html file is among the 
#' outputs, it adds hyperlinks to functions defined within the file to make it 
#' easier for someone reading the code to see where different functions are
#' defined.
#' 
#' @param input The input file to be rendered (see `rmarkdown::render`)
#' @param ... Additional parameters to pass to `rmarkdown::render`
#' @export
render <- function(input, ...) {
  # call rmarkdown::render in a new environment so it behaves the same as 
  # pressing the knit button in RStudio:
  # https://bookdown.org/yihui/rmarkdown-cookbook/rmarkdown-render.html
  args <- list(...)
  out <- xfun::Rscript_call(
    rmarkdown::render,
    c(input = input, args)
  )
  
  # add hyperlinks within html output to make it easier to navigate:
  if (any(stringr::str_detect(out, "html$"))) {
    html_file <- stringr::str_subset(out, "html$")
    add_function_hyperlinks(html_file)
  }
  
  # add litr hash so we can tell later if package files were manually edited:
  params <- get_params_used(input, args$params)
  package_dir <- ifelse(
    params$package_parent_dir == ".",
    file.path(dirname(input), params$package_name),
    file.path(dirname(input), params$package_parent_dir, params$package_name)
  )
  write_hash_to_description(package_dir)
}

#' Get parameter values used in rendering
#' 
#' When the `params` argument of `rmarkdown::render()` is explicitly used, this
#' overrides the default that appears in `input`.
#' @param input The input file to be rendered (see `rmarkdown::render`)
#' @param passed_params The list of parameters that were passed to `render`.
get_params_used <- function(input, passed_params) {
  params <- rmarkdown::yaml_front_matter(input)$params
  for (param in names(passed_params)) {
    params[[param]] <- passed_params[[param]]
  }
  params
}

#' Code for setup chunk
#' 
#' Creates directory where package will be. (Deletes what is currently there as 
#' long as it appears to have been created by litr and does not have any 
#' subsequent manual edits.)  Sets the root directory to this directory and 
#' sets up the main chunk hook `litr::send_to_package` that sends code to the R 
#' package directory.
#' @param package_dir Directory where R package will be created
#' @export
setup <- function(package_dir) {
  if (file.exists(package_dir)) {
    unedited <- tryCatch(check_unedited(package_dir),
                         error = function(e) {
                           # contents of package_dir does not resemble
                           # a litr package
                           return(FALSE)
                           })
    if (unedited) unlink(package_dir, recursive = TRUE)
    else 
      stop(stringr::str_glue("The directory {normalizePath(package_dir)} already"),
           " exists and either was not created by litr or may have manual",
           " edits. In either case, please rename that",
           " directory (or delete it) and then try again.")
  }
  fs::dir_create(package_dir)
  knitr::opts_knit$set(root.dir = package_dir) # sets wd of future chunks
  knitr::knit_hooks$set(send_to_package = litr::send_to_package)
  knitr::opts_chunk$set(send_to_package = TRUE)
}

