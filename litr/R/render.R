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
  # Pre-knit steps start
  # need to specify output file and directory to be the directory of the input
  output_file <- paste0(fs::path_ext_remove(input), ".html")
  input_dir <- fs::path_dir(input)
    
  rmd_file <- readLines(input)
  preprocessed_rmd <- preprocess_chunk_labels(rmd_file)
  # write this to a temp file in the same directory as the input file
  temp_file <- paste0(fs::path_ext_remove(input),"_TMP.", fs::path_ext(input))
  modified_input <- file.path(input_dir, fs::path_file(temp_file))
  writeLines(preprocessed_rmd, modified_input)
  # Pre-knit steps end
  
  # call rmarkdown::render in a new environment so it behaves the same as 
  # pressing the knit button in RStudio:
  # https://bookdown.org/yihui/rmarkdown-cookbook/rmarkdown-render.html

  args <- list(...)
  # only change the output file name if output_file is not passed by the user
  args[["output_file"]] <- ifelse(is.null(args[["output_file"]]), output_file, args[["output_file"]])
  params <- get_params_used(modified_input, args$params)
  package_dir <- ifelse(
    params$package_parent_dir == ".",
    file.path(dirname(input), params$package_name),
    file.path(dirname(input), params$package_parent_dir, params$package_name)
  )
  args$package_dir <- package_dir

  render_ <- function(input, package_dir, ...) {
    litr::setup(package_dir)
    rmarkdown::render(input, ...)
  }

  out <- xfun::Rscript_call(render_, c(input = modified_input, args))
  
  # add hyperlinks within html output to make it easier to navigate:
  if (any(stringr::str_detect(out, "html$"))) {
    html_file <- stringr::str_subset(out, "html$")
    add_function_hyperlinks(html_file)
    add_chunk_label_hyperlinks(html_file)
  }
  
  # add to DESCRIPTION file the version of litr used to create package:
  write_version_to_description(package_dir)
  
  # add litr hash so we can tell later if package files were manually edited:
  write_hash_to_description(package_dir)
  
  # now that we've finished using the temporary file, let's clean up after our selves
  fs::file_delete(modified_input)
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

#' Add hyperlinks to embedded chunks
#' 
#' Finds chunks that are referenced in the html file by looking for comments
#' of the form `###"foo"###` and then wraps `foo` in a `span` tag with `id="foo"` 
#' and then whenever the chunk label `@@foo@@` is found it wraps a `a href="#foo"` tag so that it be
#' a hyperlink to `foo`'s definition.
#' 
#' @param html_file File name of html file that was created from Rmd file
#' @param output_file File name to output to. Default: `html_file`
#' @export
add_chunk_label_hyperlinks <- function(html_file, output_file=html_file){
    txt <- readLines(html_file)
    start_line <- which(txt == "<body>")
    pattern <- '###&quot;([a-zA-Z0-9-_.]+)&quot;###'
    # find chunks that are defined in this file:
    chunk_names <- character(0)
    for (i in seq(start_line + 1, length(txt))) {
    chunk_name <- stringr::str_match(txt[i], pattern)[, 2]
    if(is.na(chunk_name)) next
    # a function was defined in this line, so put a span around it
    txt[i] <- stringr::str_replace(
      txt[i],
      pattern,
      stringr::str_glue("<span id='{chunk_name}'>###&quot;\\1&quot;###</span>")
    )
    # and keep track of it for later:
    chunk_names <- c(chunk_names, chunk_name)
    }
    
    # whenever one of these named chunks is referenced, link to its definition
    txt <- stringr::str_replace_all(
    txt,
    paste0("@@", chunk_names, "@@", collapse = "|"),
    function(chunk_name) {
      chunk_name <- stringr::str_remove_all(chunk_name, "@@")
      stringr::str_glue("<a href='#{chunk_name}'>&lt&lt{chunk_name}&gt&gt</a>")
    }
    )
    writeLines(txt, con = output_file)
}

#' Find an Rmd chunk label in a code chunk
#' 
#' @param chunk_code Character vector of code from an Rmd code chunk. Each element is a line of the code chunk.
#' @return List where chunk_idx is a logical vector for each line of the chunk corresponding to whether a chunk label of the form <<label>> was found and chunk_ids is a character vector of chunk label was found in that chunk.
find_labels <- function(chunk_code){
    rc <- knitr:::all_patterns$md$ref.chunk
    chunk_idx <- any(idx = grepl(rc, chunk_code))
    chunk_ids <- stringr::str_trim(sub(rc, "\\1", chunk_code[grepl(rc, chunk_code)]))
    return(list(chunk_idx = chunk_idx, chunk_ids = chunk_ids))
}

#' Replace the delimiter of Rmd chunk label in a code chunk
#' 
#' @param chunk_code Character vector of code from an Rmd code chunk. Each element is a line of the code chunk.
#' @param label_delim Delimiter to replace the chunk label delimiter recognized by `knitr`. Default label delimiter is "@@"
#' @return Character vector with replaced chunk label delimiter.
replace_label_delimiter <- function(chunk_code, label_delim="@@"){
    # modified version of knitr:::all_patterns$md$ref.chunk
    rc <- "^(\\s*)(<<(.+)>>)(\\s*)$"
    idx <- grepl(rc, chunk_code)
    # we want to keep the original indentation so we insert the indentation to the left and right of the label
    sub(rc, "\\1@@\\3@@\\4", chunk_code)
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
#' @param type Whether this is a R/ file, man/ file, or a c file
do_not_edit_message <- function(rmd_file, type = c("R", "man", "c")) {
  if (type[1] == "R")
    return(stringr::str_glue("# Generated from {rmd_file}: do not edit by hand"))
  else if (type[1] == "man")
    return(stringr::str_glue("% Please edit documentation in {rmd_file}."))
  else if (type[1] == "c")
    return(stringr::str_glue("// Generated from {rmd_file}: do not edit by hand"))
  else
    stop("type must be either 'R', 'man', or 'c'.")
}

#' Generate litr version field name for DESCRIPTION file
description_litr_version_field_name <- function() return("LitrVersionUsed")

#' Write the version of litr used to the DESCRIPTION file
#' 
#' @param package_dir Path to package
write_version_to_description <- function(package_dir) {
  ver <- as.character(utils::packageVersion("litr"))
  add_text_to_file(
    txt = stringr::str_glue("{description_litr_version_field_name()}: {ver}"),
    filename = file.path(package_dir, "DESCRIPTION"),
    req_exist = TRUE
    )
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
  # remove _TMP from the file name in the message
  msg <- stringr::str_replace(msg, "_TMP.Rmd", ".Rmd")
  for (fname in fs::dir_ls("man")) {
    txt <- stringr::str_replace(readLines(fname), pattern, msg)
    cat(paste(txt, collapse = "\n"), file = fname)
  }
}
