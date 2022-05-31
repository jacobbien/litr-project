# Generated from create-litr.Rmd: do not edit by hand

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
