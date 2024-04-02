# Generated from _main.Rmd: do not edit by hand

#' Generate do-not-edit message to put at top of file
#' 
#' @param rmd_file Name of the Rmd file to mention
#' @param type Whether this is a R/ file, man/ file, or a c file
#' @keywords internal
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

#' Use roxygen to document a package from within a Rmd file
#' 
#' This is a wrapper for the `devtools::document()` function, which in turn is a
#' wrapper for the `roxygen2::roxygenize()` function.  It is written assuming that
#' it is being called from within a generating Rmd file.  The purpose for `litr` 
#' having this wrapper is two-fold.  First, it ensures that the first line
#' in the outputted `Rd` files should not say "Please edit documentation in 
#' R/file.R" but instead should refer to the Rmd file that generates everything. 
#' Second, in the case that Rcpp is being used, it makes some adjustments to ensure
#' that the compiling of the C++ code should be successful.
#' 
#' @param ... Arguments to be passed to `devtools::document()`
#' @export
document <- function(...) {
  # prepare Rcpp code for compiling
  if (fs::file_exists("src/code.cpp")) {
    # make sure that #include <RcppArmadillo.h> if it exists
    # comes *before* (or instead of) <Rcpp.h>
    txt <- readLines("src/code.cpp")
    loc <- stringr::str_which(txt, r"(#include <RcppArmadillo.h>)")
    if (length(loc) > 0) {
      include_arma_line <- txt[loc[1]]
      txt <- c(include_arma_line, txt[-loc])
      writeLines(txt, "src/code.cpp")
    }
  }
  
  devtools::document(...)
  # remove the line of the following form in each man/*.Rd file:
  pattern <- "% Please edit documentation in .*$"
  msg <- do_not_edit_message(knitr::current_input(), type = "man")
  for (fname in fs::dir_ls("man")) {
    txt <- stringr::str_replace(readLines(fname), pattern, msg)
    cat(paste(txt, collapse = "\n"), file = fname)
  }
}

#' Modify an existing output format to have `litr` behavior
#' 
#' This function modifies the `pre_knit()` and `post_processor()` functions of a
#' preexisting output format so that it will have the `litr` behavior (meaning that an R package will be created when `rmarkdown::render()` is called).
#' 
#' @param base_format a preexisting, non-litr output format such as `rmarkdown::html_document`
#' @param minimal_eval If `TRUE`, then only chunks with `litr::document()` or 
#' `usethis` commands will be evaluated.  This can be convenient in coding when 
#' you just want to quickly update the R package without having to wait for long
#' evaluations to occur.
#' @export
litrify_output_format <- function(base_format = rmarkdown::html_document,
                                  minimal_eval = FALSE) {
  force(base_format) # I think using force here is advisable?
  force(minimal_eval) # https://adv-r.hadley.nz/function-factories.html
  function(...) {
    old <- base_format(...)
    new <- old
    new$original_knitr_objects <- list()
    new$pre_knit <- function(...) {
      args <- list(...)
      input <- args$input
      params <- knitr::knit_params(readLines(input))
      package_dir <- get_package_directory(
        params$package_parent_dir$value,
        params$package_name$value,
        input)
      new$original_knitr_objects <<- litr:::setup(package_dir, minimal_eval)
      if (!is.null(old$pre_knit)) old$pre_knit(...)
    }

    new$post_processor <- function(metadata, input_file, output_file, ...) {
      # typically the post_processor function returns the output file path
      # if old$post_processor is NULL, as in the case of pdf_document,
      # then R will throw an error when trying to call old$post_processor
      # if we only add a check for non null old$post_processor and otherwise
      # set out <- NULL then R will throw an error later in rmarkdown::render
      # since output_file is set to the output of the post_processor if 
      # output_format$post_processor is not null (See line 478 in rmarkdown::render)
      # Therefore, our solution is to set out to the output_file path if old$post_process is null.
      if (!is.null(old$post_processor)){
        out <- old$post_processor(metadata, input_file, output_file, ...)  
      } else {
        out <- output_file 
      }
      package_dir <- get_package_directory(
        metadata$params$package_parent_dir,
        metadata$params$package_name,
        input_file
      )
      # remove .Rproj and .gitignore if usethis::create_package() added these
      remove_rstudio_extras(package_dir)

      # add to DESCRIPTION file the version of litr used to create package:
      write_version_to_description(package_dir)

      # add litr hash so we can tell later if package files were manually edited:
      write_hash_to_description(package_dir)
      
      out
    }
    
    new$on_exit <- function() {
      old$on_exit()
      
      # restore knitr to its original state
      restore_knitr_objects(new$original_knitr_objects)
    }
    
    # mark this as a litr_format
    new$litr_format <- TRUE
    
    # litr formats have minimal_eval as an option
    new$minimal_eval <- minimal_eval

    new
  }
}

#' Generate litr version field name for DESCRIPTION file
#' @keywords internal
description_litr_version_field_name <- function() return("LitrVersionUsed")

#' Write the version of litr used to the DESCRIPTION file
#' 
#' @param package_dir Path to package
#' @keywords internal
write_version_to_description <- function(package_dir) {
  ver <- as.character(utils::packageVersion("litr"))
  add_text_to_file(
    txt = stringr::str_glue("{description_litr_version_field_name()}: {ver}"),
    filename = file.path(package_dir, "DESCRIPTION"),
    req_exist = TRUE
    )
}

#' Get package directory
#' 
#' @param package_parent_dir The directory of where the package should go (relative to the input directory)
#' @param package_name The name of the package
#' @param input The file name of the input
#' @keywords internal
get_package_directory <- function(package_parent_dir, package_name, input) {
  if (package_parent_dir == ".")
    return(file.path(dirname(input), package_name))
  file.path(dirname(input), package_parent_dir, package_name)
}

#' litr version of `rmarkdown::pdf_document()`
#' 
#' This behaves exactly like `rmarkdown::pdf_document()` except it creates an 
#' R package.
#' 
#' @param minimal_eval If `TRUE`, then only chunks with `litr::document()` or 
#' `usethis` commands will be evaluated.  This can be convenient in coding when 
#' you just want to quickly update the R package without having to wait for long
#' evaluations to occur.
#' @param ... Parameters to be passed to `rmarkdown::pdf_document()` 
#' @export
litr_pdf_document <- function(minimal_eval = FALSE, ...) {
  litr_pdf_document_ <- litrify_output_format(rmarkdown::pdf_document,
                                              minimal_eval = minimal_eval)
  old <- litr_pdf_document_(...)
  new <- old

  # post_knit
  new$post_knit = function(...){
    args = list(...)
    input_filename <- args[[2]]
    knitted_filename <- fs::path_ext_set(input_filename, ".knit.md")
    knitted_output <- readLines(knitted_filename)
    cleaned_output <- sapply(1:length(knitted_output), function(i){
      test_str <- knitted_output[i]
      fansi:::VAL_IN_ENV(x=test_str, ctl="all", warn=TRUE, warn.mask=fansi:::get_warn_mangled())
      .Call(fansi:::FANSI_strip_csi, test_str, CTL.INT, WARN.INT)
    })
    writeLines(cleaned_output, knitted_filename)
  }
  new
}

#' litr version of `rmarkdown::html_document()`
#' 
#' This behaves like `rmarkdown::html_document()` with a few differences:
#' - It creates an R package.
#' - It adds hyperlinks to function definitions whenever a function is used
#' elsewhere in the document.
#' - It does "Knuth-style" chunk referencing with hyperlinks.
#' 
#' @param minimal_eval If `TRUE`, then only chunks with `litr::document()` or 
#' `usethis` commands will be evaluated.  This can be convenient in coding when 
#' you just want to quickly update the R package without having to wait for long
#' evaluations to occur.
#' @param ... Parameters to be passed to `rmarkdown::pdf_document()` 
#' @export
litr_html_document <- function(minimal_eval = FALSE, ...) {
  litr_html_document_ <- litrify_output_format(rmarkdown::html_document,
                                               minimal_eval = minimal_eval)
  old <- litr_html_document_(...)
  new <- old
  # modify post_processor
  new$post_processor = function(metadata, input_file, output_file, ...) {
    out <- old$post_processor(metadata, input_file, output_file, ...)
    # html_files <- fs::dir_ls(fs::path_dir(out), regexp = ".html$")
    # add hyperlinks within html output to make it easier to navigate:
    add_function_hyperlinks(output_file, metadata$params$package_name)
    add_chunk_label_hyperlinks(output_file)
    # replace ANSI sequences with HTML tag equivalents
    replace_ansi_sequences(output_file)
    out
  }
  new
}

#' Add hyperlinks to function definitions
#' 
#' Finds functions that are defined in the html file(s) by looking for text of the 
#' form `foo <- function(` and then wraps `foo` in a `span` tag with `id="foo"` 
#' and then whenever `foo` is found it wraps a `a href="file.html#foo"` tag so 
#' that it will be a hyperlink to `foo`'s definition.
#' 
#' @param html_files Character vector of file names of html files that were created
#' from Rmd files
#' @param pkg_name Name of the package created by litr. Taken from YAML front matter
#' @keywords internal
add_function_hyperlinks <- function(html_files, pkg_name) {
  find_function_defs <- function(html_file) {
    txt <- readLines(html_file)
    start_line <- which(txt == "<body>")
    pattern1 <- '([a-zA-Z0-9_.]+)(\\s*&lt;-\\s*function)'
    pattern2 <- stringr::str_replace(pattern1,
                                     '&lt;-',
                                     '<span class="ot">&lt;-</span>')
    pattern2 <- stringr::str_replace(pattern2,
                                     'function',
                                     '<span class="cf">function</span>')
    # find functions that are defined in this file:
    function_names <- character(0)
    for (pattern in c(pattern1, pattern2)) {
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
    }
    list(function_names = function_names, txt = txt)
  }
  fdefs <- lapply(html_files, find_function_defs)
  all_function_names <- unlist(lapply(fdefs, function(lst) lst$function_names))
  # if a function is defined multiple times, then it's ambiguous where to link to
  # so let's not try linking to it (this can occur when a function is defined 
  # within a function, such as `new$post_processor()`)
  repeated <- names(which(table(all_function_names) > 1))
  all_function_names <- setdiff(all_function_names, repeated)
  if (length(all_function_names) == 0) {
    # no functions defined in package, so nothing more to be done here
    return()
  }
  num_per_file <- unlist(lapply(fdefs, 
                                function(lst) {
                                  length(setdiff(lst$function_names, repeated))
                                }))
  where_defined <- rep(fs::path_file(html_files), times = num_per_file)
  defined_functions_pattern <- paste0("(::)?",all_function_names, "\\(", collapse = "|")
  # There's also this case: <span class="fu">myfunction</span>
  defined_functions_pattern2 <- paste0(
    '<span class="fu">', all_function_names, '</span>\\(',
    collapse = "|")
  
  for (i in seq_along(html_files)) {
    # whenever one of the defined functions is named, link to its definition
    # using the format `file_where_foo_is_defined.html#foo`
    modified_txt <- insert_hrefs(fdefs[[i]]$txt, defined_functions_pattern,
                                 where_defined, all_function_names, pkg_name)
    modified_txt <- insert_hrefs(modified_txt, defined_functions_pattern2,
                                 where_defined, all_function_names, pkg_name, remove_span=TRUE)
    writeLines(modified_txt, con = html_files[i])
  }
}

#' Replace a function's name with a link to its definition
#' 
#' A helper function for `add_function_hyperlinks` that wraps references to a 
#' function in an anchor tag with a link to the function's definition.
#' 
#' @param txt Character vector where each element is a row of the knitted HTML file.
#' @param function_pattern Regular Expression passed from `add_function_hyperlinks` that contains all referenced functions in the document.
#' @param where_defined Character vector that contains the name of the file in which a function was defined.
#' @param all_function_names Character vector of all referenced functions in the document.
#' @param pkg_name Name of the package created by litr. Taken from YAML front matter.
#' @param remove_span Boolean argument for removing span tags. Used for minimizing code duplication.
#' @keywords internal
insert_hrefs <- function(txt, function_pattern, where_defined,
                         all_function_names, pkg_name, remove_span=FALSE){
  # filter down matches of defined_functions_pattern
  has_fn_name <- which(stringr::str_detect(txt, function_pattern))
  has_colon_prefix <- which(stringr::str_detect(txt, paste0("::", all_function_names, "\\(", collapse = "|")))
  has_only_fn_name <- setdiff(has_fn_name, has_colon_prefix)
  has_pkg_colon_prefix <- which(stringr::str_detect(txt, paste0(stringr::str_glue("{pkg_name}::"))))
  
  # define different replacement functions for colon prefix cases and regular cases
  colon_pref_replace_fn <- function(x){
    if(remove_span){
      fn_name <- stringr::str_remove(x, "</span>\\(")
      fn_name <- stringr::str_remove(fn_name, '<span class="fu">')
    } else{
      fn_name <- stringr::str_remove(x, "\\(")
    }
    fn_name <- stringr::str_remove(fn_name, stringr::str_glue('{pkg_name}::'))
    # implicitly assuming that a function is not redefined in another file
    def_file <- where_defined[all_function_names == fn_name]
    return(stringr::str_glue("{pkg_name}::<a href='{def_file}#{fn_name}'>{fn_name}</a>("))
    
  }
  regular_replace_fn <- function(x){
    if(remove_span){
      fn_name <- stringr::str_remove(x, '</span>\\(')
      fn_name <- stringr::str_remove(fn_name, '<span class="fu">')  
    } else {
      fn_name <- stringr::str_remove(x, "\\(")
    }
    # implicitly assuming that a function is not redefined in another file
    def_file <- where_defined[all_function_names == fn_name]
    stringr::str_glue("<a href='{def_file}#{fn_name}'>{fn_name}</a>(")
  }  
  
  colon_prefix_function_pattern <- paste0(stringr::str_glue("{pkg_name}::"),all_function_names, "\\(", collapse = "|")
  colon_prefix_refs <- stringr::str_replace_all(
    txt[has_pkg_colon_prefix],
    colon_prefix_function_pattern,
    colon_pref_replace_fn
  )
  
  regular_refs <- stringr::str_replace_all(
    txt[has_only_fn_name],
    function_pattern,
    regular_replace_fn
  )
  # now put back in the changed lines
  txt[has_pkg_colon_prefix] <- colon_prefix_refs
  txt[has_only_fn_name] <- regular_refs
  txt
}

#' Add hyperlinks to embedded chunks
#' 
#' Finds chunks that are referenced in the html file(s) by looking for comments
#' of the form `###"foo"###` and then wraps `foo` in a `span` tag with `id="foo"` 
#' and then whenever the chunk label `<<foo>>` is found it wraps it in a 
#' `a href="file.html#foo"` tag so that it will be a hyperlink to `foo`'s 
#' definition.
#' 
#' @param html_files Character vector of file names of html files that were created
#' from Rmd files
#' @param reference_start The delimiter used to indicate the start of a chunk label 
#' @param reference_end The delimiter used to indicate the end of a chunk label 
#' @keywords internal
add_chunk_label_hyperlinks <- function(html_files,
                                       reference_start = "&lt;&lt;",
                                       reference_end = "&gt;&gt;"){
  find_chunk_defs <- function(html_file) {
    txt <- readLines(html_file)
    start_line <- which(txt == "<body>")
    pattern <- '###&quot;([a-zA-Z0-9-_.]+)&quot;###'
    # find chunks that are defined in this file:
    chunk_names <- character(0)
    for (i in seq(start_line + 1, length(txt))) {
      chunk_name <- stringr::str_match(txt[i], pattern)[, 2]
      if(is.na(chunk_name)) next
      # a chunk was defined in this line, so put a span around it
      txt[i] <- stringr::str_replace(
        txt[i],
        pattern,
        stringr::str_glue("<span id='{chunk_name}'>###&quot;\\1&quot;###</span>")
      )
      # and keep track of it for later.
      # we're using setNames here to make sure that we keep the name of file
      # where the chunk name is defined 
      chunk_names <- setNames(c(chunk_names, chunk_name), c(names(chunk_names), html_file))
    }
    list(chunk_names = chunk_names, txt = txt)
  }
  
  cdefs <- lapply(html_files, find_chunk_defs)
  all_chunk_names <- unlist(lapply(cdefs, function(lst) lst$chunk_names))
  num_per_file <- unlist(lapply(cdefs, function(lst) length(lst$chunk_names)))
  where_defined <- rep(fs::path_file(html_files), times = num_per_file)
  
  defined_chunks_pattern <- paste0(reference_start, all_chunk_names, reference_end, 
                                   collapse = "|")
  ref_start <- '<span class="sc">&lt;</span><span class="er">&lt;</span>'
  ref_start_alt <- '<span class=\"er\">&lt;&lt;</span>'
  ref_end <- '<span class="sc">&gt;</span><span class="er">&gt;</span>'
  hyphen_with_extras <- '<span class="sc">-</span>'
  all_chunk_names2 <- stringr::str_replace_all(all_chunk_names, "-", hyphen_with_extras)
  defined_chunks_pattern2 <- paste0(
    ref_start, all_chunk_names2, ref_end, collapse = "|"
    )
  defined_chunks_pattern2_alt <- paste0(
    ref_start_alt, all_chunk_names2, ref_end, collapse = "|"
    )
  defined_chunks_pattern2 <- paste(
    defined_chunks_pattern2, defined_chunks_pattern2_alt, sep = "|"
    )

  for (i in seq_along(html_files)) {
    # whenever one of these named chunks is referenced, link to its definition
    # using the format `file_where_chunk_is_defined.html#chunkname`
    txt <- stringr::str_replace_all(
      cdefs[[i]]$txt,
      defined_chunks_pattern,
      function(x) {
        cname <- stringr::str_remove_all(
          x,
          paste(reference_start, reference_end, sep = "|")
        )
        def_file <- where_defined[all_chunk_names == cname]
        stringr::str_glue(
          "<a href='{def_file}#{cname}'>{reference_start}{cname}{reference_end}</a>"
          )
      }
    )
    txt <- stringr::str_replace_all(
      txt,
      defined_chunks_pattern2,
      function(x) {
        cname <- stringr::str_remove_all(
          x,
          paste(ref_start, ref_start_alt, ref_end, sep = "|")
        )
        def_file <- where_defined[all_chunk_names2 == cname]
        cname <- stringr::str_replace_all(cname, hyphen_with_extras, "-")
        stringr::str_glue(
          "<a href='{def_file}#{cname}'>{reference_start}{cname}{reference_end}</a>"
          )
      }
    )

    parsed_html <- xml2::read_html(paste(txt,collapse="\n"))
    # get all possible chunk names in this file.
    chunk_names <- all_chunk_names[which(names(all_chunk_names) == html_files[i])]
    
    if(length(chunk_names) > 0){
      for(j in seq_along(chunk_names)){
        span_node <- xml2::xml_find_first(parsed_html, stringr::str_glue('(.//span[@id="{chunk_names[j]}"])'))
        span_node_path <- stringr::str_split(xml2::xml_path(span_node),"/")
        
        pre_path <- paste(span_node_path[[1]][1:(max(which(stringr::str_detect(span_node_path[[1]], "pre"))))],collapse="/")
        if(nchar(pre_path) == 0){
          next()
        }
        pre_parent <- xml2::xml_find_first(parsed_html, pre_path)
        if(is.na(pre_parent)){
          next()
        }
        xml2::xml_add_parent(pre_parent
                             , xml2::read_xml(stringr::str_glue('<fieldset id="{chunk_names[j]}" class="chunkfield"> </fieldset>')))
        xml2::xml_add_sibling(pre_parent, xml2::read_xml(stringr::str_glue('<legend class="chunklegend">{chunk_names[j]}</legend>')), where="before")
        xml2::xml_remove(span_node)
        # remove the extra line break that is left over from removing the span
        code_node <- xml2::xml_child(pre_parent)
        changed_txt <- stringr::str_remove(paste(as.character(xml2::xml_contents(code_node)),collapse=""), '\n')
        xml2::xml_replace(code_node, xml2::read_xml(stringr::str_glue('<code>{changed_txt}</code>')))
      }
    }
    # last thing is to insert an additional style node in the head with our CSS so we have a standard style whether we are using bookdown or Rmarkdown
    css_string <- "fieldset.chunkfield {border:1px dotted black;padding-bottom: 0px;padding-top: 0px;margin:0 2px;padding:.35em .625em .75em}
    legend.chunklegend {padding:0;width:auto;border:0; border-bottom: none; margin-bottom:0}
    "
    head_node <- xml2::xml_find_first(parsed_html, ".//head")
    xml2::xml_add_child(head_node, xml2::read_xml(stringr::str_glue("<style type='text/css'>{css_string}</style>")))
    txt <- xml2::write_html(parsed_html, html_files[i])
  }
}

#' Replace ANSI escape sequences with their HTML equivalents
#' 
#' Finds ANSI escape sequences and replaces them with HTML tags using the `fansi` package
#' 
#' @param html_files Character vector of file names of html files that were created
#' from Rmd files
#' @keywords internal
replace_ansi_sequences <- function(html_files) {
  for (i in seq_along(html_files)) {
    file_lines <- readLines(html_files[i])
    # look for lines with escape sequences for URLs and remove the URL
    # escape sequences before we convert to HTML
    url_code_regex <- "\\033]8;;.*\\a(.*?)\\033]8;;\\a"
    url_seq_idx <- which(stringr::str_detect(file_lines, url_code_regex))
    file_lines[url_seq_idx] <- sapply(url_seq_idx, function(idx){
      line <- file_lines[idx]
      stringr::str_replace(line, url_code_regex, stringr::str_glue("\\1"))  
    })
    
    txt <-
      fansi::sgr_to_html(x = file_lines,
                         warn = FALSE,
                         term.cap = "256")
    writeLines(txt, con = html_files[i])
  }
}

#' litr version of `bookdown::gitbook()`
#' 
#' This behaves like `bookdown::gitbook()` with a few differences:
#' - It creates an R package.
#' - It adds hyperlinks to function definitions whenever a function is used
#' elsewhere in the document.
#' - It does "Knuth-style" chunk referencing with hyperlinks.
#' 
#' @param minimal_eval If `TRUE`, then only chunks with `litr::document()` or 
#' `usethis` commands will be evaluated.  This can be convenient in coding when 
#' you just want to quickly update the R package without having to wait for long
#' evaluations to occur.
#' @param ... Parameters to be passed to `bookdown::gitbook()` 
#' @export
litr_gitbook <- function(minimal_eval = FALSE, ...) {
  litr_gitbook_ <- litrify_output_format(bookdown::gitbook,
                                         minimal_eval = minimal_eval)
  old <- litr_gitbook_(...)
  new <- old
  # modify post_processor
  new$post_processor = function(metadata, input_file, output_file, ...) {
    out <- old$post_processor(metadata, input_file, output_file, ...)
    out_dir <- fs::path_dir(out)
    file_stems <- readLines(file.path(out_dir, "reference-keys.txt"))
    html_files <- file.path(out_dir, paste0(file_stems, ".html"))
    html_files <- unique(intersect(c(out, html_files), fs::dir_ls(out_dir)))
    # add hyperlinks within html output to make it easier to navigate:
    add_function_hyperlinks(html_files, metadata$params$package_name)
    add_chunk_label_hyperlinks(html_files)
    # replace ANSI sequences with HTML tag equivalents
    replace_ansi_sequences(html_files)
    out
  }
  new
}

#' Render R markdown file
#' 
#' Wrapper to `rmarkdown::render()` that produces an R package as output in 
#' addition to the standard output document.  It does some post-processing on the 
#' .html file when that is the output.  In particular, when an .html file is among
#' the outputs, it adds hyperlinks to functions defined within the file to make 
#' it easier for someone reading the code to see where different functions are
#' defined.
#' 
#' @param input The input file to be rendered (see `rmarkdown::render`)
#' @param minimal_eval If `TRUE`, then only chunks with `litr::document()` or 
#' `usethis` commands will be evaluated.  This can be convenient in coding when 
#' you just want to quickly update the R package without having to wait for long
#' evaluations to occur.
#' @param fresh_session Whether to call `rmarkdown::render` from a fresh R 
#' session. By default TRUE, so that it matches the behavior of pressing "Knitr"
#' in RStudio.  However, for debugging it can be useful to set this to FALSE so 
#' that functions like `debug()` and `browser()` will work.
#' @param ... Additional parameters to pass to `rmarkdown::render`
#' @export
render <- function(input, minimal_eval, fresh_session = TRUE, ...) {
  # call rmarkdown::render in a new environment so it behaves the same as 
  # pressing the knit button in RStudio:
  # https://bookdown.org/yihui/rmarkdown-cookbook/rmarkdown-render.html
  args <- list(...)

  # let's determine if the output format being used is a litr format.
  # If it is, then we'll simply want to call rmarkdown::render() since the 
  # special litr behavior will be attained through the output format.
  litr_format <- FALSE
  bookdown_format <- FALSE
  output_format_arg <- FALSE
  if ("output_format" %in% names(args)) {
    output_format_arg <- TRUE
    if ("litr_format" %in% names(args$output_format)) {
      litr_format <- TRUE
    }
    if ("bookdown_output_format" %in% names(args$output_format)) {
      bookdown_format <- TRUE
    }
  } else {
    frontmatter <- rmarkdown::yaml_front_matter(input)
    if ("output" %in% names(frontmatter)) {
      formats <- ifelse(is.list(frontmatter$output),
                        names(frontmatter$output),
                        frontmatter$output)
      if (any(stringr::str_detect(formats, "litr::"))) {
        litr_format <- TRUE
      }
      if (any(stringr::str_detect(formats, "litr::litr_gitbook"))) {
        bookdown_format <- TRUE
      }
    }
  }

  # get package_directory
  params <- get_params_used(input, args$params)
  package_dir <- get_package_directory(
    params$package_parent_dir,
    params$package_name,
    input
    )
  
  # if minimal_eval was passed to render, add this to the output_options
  # argument that will be passed to rmarkdown::render
  if (is.null(args$output_options)) args$output_options <- list()
  if (!missing(minimal_eval)) args$output_options$minimal_eval <- minimal_eval
  
  # determine whether a new R session will be created when we run the rendering 
  # function of rmarkdown/bookdown
  if (fresh_session)
    run_function <- xfun::Rscript_call
  else
    run_function <- do.call
  
  if (litr_format) {
    # this uses a litr output format, so we don't need to do anything litr-specific
    # here because it will happen through the output format
    
    if (output_format_arg & !missing(minimal_eval)) {
      # the output format was passed through the output_format argument rather 
      # than through the metadata
      if (minimal_eval) {
        stop(make_noticeable(paste(
          "When passing a litr output format using the output_format argument,",
          "you should not pass minimal_eval = TRUE directly to render.",
          "Instead, pass it to the litr output format function.  For example,",
          "litr::litr_html_document(minimal_eval = TRUE).",
          collapse = " "
          )))
      }
    }
    
    if (bookdown_format) {
      if (fs::is_file(input)) input <- fs::path_dir(input)
      return(invisible(run_function(with_cleanup(bookdown::render_book,
                                                 package_dir),
                                    c(input = input, args))))
    }
    else
      return(invisible(run_function(with_cleanup(rmarkdown::render,
                                                 package_dir),
                                    c(input = input, args))))
  }
  
  # the output format being used is not a litr-specific one, so we need to make
  # sure that all the special litr things happen
  args$package_dir <- package_dir

  render_ <- function(input, package_dir, minimal_eval, ...) {
    knitr_objects <- litr:::setup(package_dir, minimal_eval)
    out <- rmarkdown::render(input, ...)
    restore_knitr_objects(knitr_objects)
    # remove .Rproj and .gitignore if usethis::create_package() added these
    remove_rstudio_extras(package_dir)
    return(out)
  }

  if (missing(minimal_eval)) minimal_eval <- FALSE
  out <- run_function(with_cleanup(render_, package_dir),
                      c(input = input, minimal_eval = minimal_eval, args))


  # add hyperlinks within html output to make it easier to navigate:
  if (any(stringr::str_detect(out, "html$"))) {
    html_file <- stringr::str_subset(out, "html$")
    add_function_hyperlinks(html_file, params$package_name)
    add_chunk_label_hyperlinks(html_file)
  }
  
  # add to DESCRIPTION file the version of litr used to create package:
  write_version_to_description(package_dir)
  
  # add litr hash so we can tell later if package files were manually edited:
  write_hash_to_description(package_dir)
}

#' Add litr hash to DESCRIPTION file if error encountered
#' 
#' This creates a function that calls the passed function within the context of
#' a try-catch.  If an error is encountered, the litr hash is still added to
#' the DESCRIPTION file so that future calls to `litr::render()` will recognize
#' that it can safely overwrite the package directory (i.e., no manual editing
#' occurred).
#' 
#' @param fun function being called
#' @param package_dir directory where package is being written to
#' @param ... arguments to be passed to `fun`
#' @keywords internal
with_cleanup <- function(fun, package_dir) {
  return(function(...) {
    withCallingHandlers(
      fun(...),
      error = function(e) {
        # add litr hash so we can tell later if package files were manually edited:
        write_hash_to_description(package_dir)
      })
  })
}

#' Return the knitr objects to their original state
#' 
#' @param original_knitr_objects As returned by `setup()`
#' @keywords internal
restore_knitr_objects <- function(original_knitr_objects) {
  knitr::opts_knit$restore(original_knitr_objects$opts_knit)
  knitr::knit_hooks$restore(original_knitr_objects$knit_hooks)
  knitr::opts_chunk$restore(original_knitr_objects$opts_chunk)
  knitr::opts_hooks$restore(original_knitr_objects$opts_hooks)
  knitr::knit_engines$restore(original_knitr_objects$knit_engines)
}

#' Remove extra files added by usethis
#' 
#' Remove .Rproj and .gitignore files if they are in the package directory.
#' 
#' @param package_dir Path to package
#' @keywords internal
remove_rstudio_extras <- function(package_dir) {
  extras <- fs::dir_ls(package_dir,
                       all = TRUE,
                       regexp = "[.]Rproj$|[.]gitignore$")
  rbuildignore <- file.path(package_dir, ".Rbuildignore")
  txt <- readLines(rbuildignore)
  txt <- stringr::str_subset(txt, "^.*Rproj.*$", negate = TRUE)
  writeLines(txt, con = rbuildignore)
  for (extra in extras) fs::file_delete(extra)
}

#' Get parameter values used in rendering
#' 
#' When the `params` argument of `rmarkdown::render()` is explicitly used, this
#' overrides the default that appears in `input`.
#' @param input The input file to be rendered (see `rmarkdown::render`)
#' @param passed_params The list of parameters that were passed to `render`.
#' @keywords internal
get_params_used <- function(input, passed_params) {
  params <- rmarkdown::yaml_front_matter(input)$params
  for (param in names(passed_params)) {
    params[[param]] <- passed_params[[param]]
  }
  params
}

#' Load complete package
#' 
#' This is a litr wrapper to `devtools::load_all()`.  It first calls
#' `litr::render()` with `minimal_eval=TRUE`, then it calls
#' `devtools::load_all()` on the generated package.
#' 
#' @param input The input file to be rendered (see `rmarkdown::render`)
#' @param output_dir By default (and in typical usage) this is NULL, meaning
#' that no .html/bookdown/.pdf will result.  However, when a directory is given,
#' the result of the litr-knitting will be saved to this location.
#' @param ... Additional parameters to be passed to `devtools::load_all()`
#' @export
load_all <- function(input, output_dir = NULL, ...) {
  no_output <- is.null(output_dir)
  if (no_output) {
    output_dir <- tempfile()
    if (fs::file_exists(output_dir)) fs::file_delete(output_dir)
    fs::dir_create(output_dir)
  }
  
  # let's copy over everything from input directory to output directory
  fs::dir_copy(fs::path_dir(input), output_dir, overwrite = TRUE)
  input_path <- fs::path_split(input)[[1]]
  moved_input <- file.path(output_dir, fs::path_file(input))
  
  # get package directory
  params <- get_params_used(moved_input, list())
  package_dir <- get_package_directory(
    params$package_parent_dir,
    params$package_name,
    moved_input
  )
  
  # but if a package directory was copied here, let's remove it before
  # calling render to avoid a potential error
  if (fs::dir_exists(package_dir)) fs::dir_delete(package_dir)
  
  litr::render(moved_input, minimal_eval = TRUE, output_dir = output_dir,
               quiet = TRUE)
  
  new_package_dir <- file.path(fs::path_dir(input), params$package_name)
  fs::dir_copy(package_dir, new_package_dir, overwrite = TRUE)
  if (no_output) fs::dir_delete(output_dir)
  
  devtools::load_all(new_package_dir)
}
