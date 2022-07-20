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

  out <-  tryCatch(
      {
        xfun::Rscript_call(render_, c(input = modified_input, args))
      },
      error=function(cond){
        # look for a _TMP version of the input file
        tmp_files <- fs::dir_ls(file.path(dirname(input)), regexp = stringr::str_replace(fs::path_file(input), ".(Rmd|rmd|RMD)","_TMP.\\1"))
        if( length(tmp_files) > 0){
          fs::file_delete(tmp_files)
        }
        stop(cond)
      }
    )
    
  # out <- xfun::Rscript_call(render_, c(input = input, args))
  
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
#' and then whenever the chunk label `@@@@@@foo@@@@@@` is found it wraps it in a 
#' `a href="#foo"` tag so that it will be a hyperlink to `foo`'s definition.
#' 
#' @param html_file File name of html file that was created from Rmd file
#' @param output_file File name to output to. Default: `html_file`
#' @param reference_delim The delimiter used to indicate a chunk label 
#' @export
add_chunk_label_hyperlinks <- function(html_file, output_file = html_file,
                                       reference_delim = "@@@"){
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
    paste0(reference_delim, chunk_names, reference_delim, collapse = "|"),
    function(chunk_name) {
      chunk_name <- stringr::str_remove_all(chunk_name, reference_delim)
      stringr::str_glue("<a href='#{chunk_name}'>&lt&lt{chunk_name}&gt&gt</a>")
    }
    )
    writeLines(txt, con = output_file)
}

#' Find an Rmd chunk label in a code chunk
#' 
#' @param chunk_code Character vector of code from an Rmd code chunk. Each element is a line of the code chunk.
#' @return List where chunk_idx is a logical vector for each line of the chunk corresponding to whether a chunk label of the form `<<label>>` was found and chunk_ids is a character vector of chunk label was found in that chunk.
find_labels <- function(chunk_code){
    rc <- knitr::all_patterns$md$ref.chunk
    chunk_idx <- any(idx = grepl(rc, chunk_code))
    chunk_ids <- stringr::str_trim(sub(rc, "\\1", chunk_code[grepl(rc, chunk_code)]))
    return(list(chunk_idx = chunk_idx, chunk_ids = chunk_ids))
}

#' Replace the delimiter of Rmd chunk label in a code chunk
#' 
#' @param chunk_code Character vector of code from an Rmd code chunk. Each element is a line of the code chunk.
#' @param label_delim Delimiter to replace the chunk label delimiter recognized by `knitr`. Default label delimiter is `@@@@@@`
#' @return Character vector with replaced chunk label delimiter.
replace_label_delimiter <- function(chunk_code, label_delim="@@@"){
    # modified version of knitr:::all_patterns$md$ref.chunk
    rc <- "^(\\s*)(<<(.+)>>)(\\s*)$"
    idx <- grepl(rc, chunk_code)
    # we want to keep the original indentation so we insert the indentation to the left and right of the label
    sub(rc, stringr::str_glue("\\1{label_delim}\\3{label_delim}\\4"), chunk_code)
}

#' Find an Rmd chunk label in a code chunk and modify the chunk reference delimiter before knitting
#' 
#' @param rmd_file A character vector of an Rmd file lines
#' @export 
preprocess_chunk_labels <- function(rmd_file){
    code_chunks <- extract_rmd_code_chunks(rmd_file)
    # handle edge case of no code chunks
    if(length(code_chunks) > 0){
      # loop over the code_chunk list 
      offset = 0
      for(i in 1:length(code_chunks)){
        # check if we found a referenced label for this element
        code_chunk <- code_chunks[[i]]
        if(code_chunk$contains_labels$chunk_idx){
          chunk_start <- code_chunk$start_idx+offset
          chunk_end <- code_chunk$end_idx+offset
          orig_chunk <- rmd_file[chunk_start:chunk_end]
          dup_chunk <- orig_chunk
          # we need to add eval=F to the original chunk and change the ref brackets so that knitr doesn't fill in the labels
          # add eval=F at the end of the chunk options which we assume to be the first line of the chunk
          # TO-DO: check if it is possible to have multi-line chunk options?
          orig_chunk[1] <- stringr::str_replace(orig_chunk[1], "\\}", stringr::str_glue(", eval=FALSE\\}"))
          orig_chunk <- sapply(orig_chunk, replace_label_delimiter, USE.NAMES = FALSE)
          
          # we need to hide this chunk but still have it evaluated (include=FALSE)
          # knitr should still fill in the labels
          dup_chunk[1] <- stringr::str_replace(dup_chunk[1], "\\}", stringr::str_glue(", include=FALSE\\}"))
          dup_chunk[1] <- modify_chunk_label(dup_chunk[1])
          
          # now replace the original chunk and fill in the duplicate chunk
          # and increase the offset by the size of the duplicate chunk
          rmd_file[chunk_start:chunk_end] <- orig_chunk
          rmd_file <- append(rmd_file, dup_chunk, after=chunk_end)
          offset <- offset + length(dup_chunk)
        }
      }
      
      unique_referenced_chunks <- code_chunks |> {\(x) purrr::map(x,function(y){
          if(length(y$contains_labels$chunk_ids) > 0){
              return(y$contains_labels$chunk_ids)
          }
      })}()
      unique_referenced_chunks <- unique_referenced_chunks[!sapply(unique_referenced_chunks,is.null)] |> unlist() |> unique()
      
      all_chunk_names <- purrr::map_chr(code_chunks, "chunk_label")
      # check that the referenced chunk names exist in the file, otherwise throw error
      refs_not_in_file <- setdiff(unique_referenced_chunks,all_chunk_names)
      if(length(refs_not_in_file) > 0){
        stop(stringr::str_glue("Unable to find the following chunk reference(s) in this Rmarkdown file: {toString(refs_not_in_file)}."))
      }
      
      changed_code_chunks <- extract_rmd_code_chunks(rmd_file)
      offset = 0
      for(i in 1:length(changed_code_chunks)){
        code_chunk <- changed_code_chunks[[i]]
        chunk_start <- code_chunk$start_idx + offset
        chunk_end <- code_chunk$end_idx + offset
        orig_chunk <- rmd_file[chunk_start:chunk_end]
        chunk_label <- code_chunk$chunk_label
        if( (chunk_label != "") & chunk_label %in% unique_referenced_chunks){
          chunk_comment = stringr::str_glue('###"{chunk_label}"###')
          rmd_file <- append(rmd_file, chunk_comment, after = chunk_start)
          offset <- offset + 1
        }
      }
    }
    
    
    rmd_file
}

#' Get a list of code chunks from an Rmd file in vector form
#' 
#' @param rmd_vector A character vector of an Rmd file that has been split by new lines
#' @return A list containing information about each code chunk including the code, whether it contains references to other code chunks, and the start and end line numbers of the code chunk in the Rmd file.
extract_rmd_code_chunks <- function(rmd_vector){
  chunk_start <- which(grepl(rmd_vector, pattern = knitr::all_patterns$md$chunk.begin))
  # handle the case where no code chunks found
  if(length(chunk_start) > 0){
    chunk_end <- sapply(chunk_start, function(x){
    # starting from the chunk start, find the first triple backticks location
    num_after_start <- which(grepl(rmd_vector[-(1:x)], pattern = knitr::all_patterns$md$chunk.end, perl = T))[1]
    return(x + num_after_start)
  })
  code_extract <- function(rmd_vector, start, end){
    return(rmd_vector[(start+1):(end-1)])
  }
  # create a list of code chunks by looping over the chunk_start and chunk_end vectors
  code_chunk_list <- lapply(1:length(chunk_start), function(i){
    start <- chunk_start[i]
    end <- chunk_end[i]
    code <- code_extract(rmd_vector, start, end)
    # include the name of the current chunk here
    chunk_label <- extract_label(rmd_vector[start])
    # add an element of the list with information on whether another code chunk is referenced in this code chunk
    contains_labels <- find_labels(code)
    list(code = code, chunk_label = chunk_label, contains_labels = contains_labels, start_idx = start, end_idx = end)
  })
  } else{
    code_chunk_list <- list()
  }
  
  code_chunk_list
}

#' Extract chunk label from the first line of an Rmd chunk.
#' 
#' @param chunk_option Character vector of the first line of a chunk, i.e. ```{r chunk-name, ...}
#' @return Character vector of the chunk label if it exists, empty string "" otherwise
extract_label <- function(chunk_option){
  label <- stringr::str_trim(gsub("^[\t >]*(```+\\s*\\{)([a-zA-Z0-9_]+)( *[ ,][a-zA-Z0-9_-]+)?(.*\\}\\s*)$", '\\3', chunk_option))
  # this regular expression returns an empty string if the chunk does not have a label so just return the label
  return(label)
}

#' Modify a chunk name and add "-dup" as a suffix.
#' This function assumes that we are passed a chunk option line that contains a chunk label since this function is called in the context of creating a duplicate version of a referenced chunk.
#' @param chunk_option Character vector of the first line of a chunk, i.e. ```{r chunk-name, ...}
#' @return Character vector of the chunk option with "-dup" appended to the chunk label. i.e., ```{r chunk-name-dup, ...}
modify_chunk_label <- function(chunk_option){
  # look for a name in the chunk option, if there isn't a name then the fourth element will be NA
  regex_matches <- stringr::str_match(chunk_option,"^[\t >]*(```+\\s*\\{)([a-zA-Z0-9_]+)( *[ ,][a-zA-Z0-9_-]+)?(.*\\}\\s*)$")
  if(!is.na(regex_matches[,4])){
    return(stringr::str_glue("{regex_matches[,2]}{regex_matches[,3]} {stringr::str_trim(regex_matches[,4])}-dup{regex_matches[,5]}"))
  } else{
    return(stringr::str_glue("{regex_matches[,2]}{regex_matches[,3]}{regex_matches[,5]}"))
  }
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
  # remove _TMP from the file name in the message
  rmd_file <- stringr::str_replace(rmd_file, "_TMP.Rmd", ".Rmd")
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
  for (fname in fs::dir_ls("man")) {
    txt <- stringr::str_replace(readLines(fname), pattern, msg)
    cat(paste(txt, collapse = "\n"), file = fname)
  }
}
