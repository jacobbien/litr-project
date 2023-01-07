# Generated from create-litr.Rmd: do not edit by hand

#' Render R markdown file
#' 
#' Wrapper to `rmarkdown::render()` that produces an R package as output in addition to the standard output document.  It does some post-processing on the 
#' html file when that is the output.  In particular, when an html file is among
#' the outputs, it adds hyperlinks to functions defined within the file to make 
#' it easier for someone reading the code to see where different functions are
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

  # let's determine if the output format being used is a litr format.
  # If it is, then we'll simply want to call rmarkdown::render() since the 
  # special litr behavior will be attained through the output format.
  litr_format <- FALSE
  bookdown_format <- FALSE
  if ("output_format" %in% names(args)) {
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

  if (litr_format) {
    # this uses a litr output format, so we don't need to do anything litr-specific
    # here because it will happen through the output format
    if (bookdown_format)
      return(invisible(xfun::Rscript_call(with_cleanup(bookdown::render_book,
                                                       package_dir),
                                          c(input = input, args))))
    else
      return(invisible(xfun::Rscript_call(with_cleanup(rmarkdown::render,
                                                       package_dir),
                                          c(input = input, args))))
  }
  
  # the output format being used is not a litr-specific one, so we need to make
  # sure that all the special litr things happen
  args$package_dir <- package_dir

  render_ <- function(input, package_dir, ...) {
    litr:::setup(package_dir)
    rmarkdown::render(input, ...)
  }

  out <- xfun::Rscript_call(with_cleanup(render_, package_dir),
                            c(input = input, args))
#  out <- do.call(with_cleanup(render_, package_dir), c(input = input, args))

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

#' Modify an existing output format to have `litr` behavior
#' 
#' This function modifies the `pre_knit()` and `post_processor()` functions of a
#' preexisting output format so that it will have the `litr` behavior (meaning that an R package will be created when `rmarkdown::render()` is called).
#' 
#' @param base_format a preexisting, non-litr output format such as `rmarkdown::html_document`
#' @export
litrify_output_format <- function(base_format = rmarkdown::html_document) {
  function(...) {
    old <- base_format(...)
    new <- old
    new$pre_knit <- function(...) {
      args <- list(...)
      input <- args$input
      params <- knitr::knit_params(readLines(input))
      package_dir <- get_package_directory(
        params$package_parent_dir$value,
        params$package_name$value,
        input)
      litr:::setup(package_dir)
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

      # add to DESCRIPTION file the version of litr used to create package:
      write_version_to_description(package_dir)

      # add litr hash so we can tell later if package files were manually edited:
      write_hash_to_description(package_dir)
      out
    }
    
    # mark this as a litr_format
    new$litr_format <- TRUE

    new
  }
}

#' litr version of `rmarkdown::pdf_document()`
#' 
#' This behaves exactly like `rmarkdown::pdf_document()` except it creates an 
#' R package.
#' 
#' @param ... Parameters to be passed to `rmarkdown::pdf_document()` 
#' @export
litr_pdf_document <- function(...) {
  litr_pdf_document_ <- litrify_output_format(rmarkdown::pdf_document)
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
#' @param ... Parameters to be passed to `rmarkdown::pdf_document()` 
#' @export
litr_html_document <- function(...) {
  litr_html_document_ <- litrify_output_format(rmarkdown::html_document)
  old <- litr_html_document_(...)
  new <- old
  # modify post_processor
  new$post_processor = function(metadata, input_file, output_file, ...) {
    out <- old$post_processor(metadata, input_file, output_file, ...)
    html_files <- fs::dir_ls(fs::path_dir(out), regexp = ".html$")
    # add hyperlinks within html output to make it easier to navigate:
    add_function_hyperlinks(html_files)
    add_chunk_label_hyperlinks(html_files)
    # replace ANSI sequences with HTML tag equivalents
    replace_ansi_sequences(html_files)
    out
  }
  new
}

#' litr version of `bookdown::gitbook()`
#' 
#' This behaves like `bookdown::gitbook()` with a few differences:
#' - It creates an R package.
#' - It adds hyperlinks to function definitions whenever a function is used
#' elsewhere in the document.
#' - It does "Knuth-style" chunk referencing with hyperlinks.
#' 
#' @param ... Parameters to be passed to `bookdown::gitbook()` 
#' @export
litr_gitbook <- function(...) {
  litr_gitbook_ <- litrify_output_format(bookdown::gitbook)
  old <- litr_gitbook_(...)
  new <- old
  # modify post_processor
  new$post_processor = function(metadata, input_file, output_file, ...) {
    out <- old$post_processor(metadata, input_file, output_file, ...)
    html_files <- fs::dir_ls(fs::path_dir(out), regexp = ".html$")
    # add hyperlinks within html output to make it easier to navigate:
    add_function_hyperlinks(html_files)
    add_chunk_label_hyperlinks(html_files)
    # replace ANSI sequences with HTML tag equivalents
    replace_ansi_sequences(html_files)
    out
  }
  new
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

#' Add hyperlinks to function definitions
#' 
#' Finds functions that are defined in the html file(s) by looking for text of the 
#' form `foo <- function(` and then wraps `foo` in a `span` tag with `id="foo"` 
#' and then whenever `foo` is found it wraps a `a href="file.html#foo"` tag so 
#' that it will be a hyperlink to `foo`'s definition.
#' 
#' @param html_files Character vector of file names of html files that were created
#' from Rmd files
#' @keywords internal
add_function_hyperlinks <- function(html_files) {
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
  defined_functions_pattern <- paste0(all_function_names, "\\(", collapse = "|")
  for (i in seq_along(html_files)) {
    # whenever one of the defined functions is named, link to its definition
    # using the format `file_where_foo_is_defined.html#foo`
    txt <- stringr::str_replace_all(
      fdefs[[i]]$txt,
      defined_functions_pattern,
      function(x) {
        fn_name <- stringr::str_remove(x, "\\(")
        def_file <- where_defined[all_function_names == fn_name]
        stringr::str_glue("<a href='{def_file}#{fn_name}'>{fn_name}</a>(")
      }
    )
    # There's also this case: <span class="fu">myfunction</span>
    defined_functions_pattern2 <- paste0(
      '<span class="fu">', all_function_names, '</span>\\(',
      collapse = "|")
    
    txt <- stringr::str_replace_all(
      txt,
      defined_functions_pattern2,
      function(x) {
        fn_name <- stringr::str_remove(x, '</span>\\(')
        fn_name <- stringr::str_remove(fn_name, '<span class="fu">')
        def_file <- where_defined[all_function_names == fn_name]
        stringr::str_glue("<a href='{def_file}#{fn_name}'>{fn_name}</a>(")
      }
    )
    writeLines(txt, con = html_files[i])
  }
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
      # and keep track of it for later:
      chunk_names <- c(chunk_names, chunk_name)
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
  ref_end <- '<span class="sc">&gt;</span><span class="er">&gt;</span></span>'
  hyphen_with_extras <- '<span class="sc">-</span>'
  all_chunk_names2 <- stringr::str_replace_all(all_chunk_names, "-", hyphen_with_extras)
  defined_chunks_pattern2 <- paste0(ref_start, all_chunk_names2, ref_end, 
                                   collapse = "|")

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
          paste(ref_start, ref_end, sep = "|")
        )
        def_file <- where_defined[all_chunk_names2 == cname]
        cname <- stringr::str_replace_all(cname, hyphen_with_extras, "-")
        stringr::str_glue(
          "<a href='{def_file}#{cname}'>{reference_start}{cname}{reference_end}</a>"
          )
      }
    )

    writeLines(txt, con = html_files[i])
  }
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
