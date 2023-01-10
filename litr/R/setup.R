# Generated from create-litr.Rmd: do not edit by hand

#' Code for setup chunk
#' 
#' * Creates directory where package will be. (Deletes what is currently there as 
#' long as it appears to have been created by litr and does not have any 
#' subsequent manual edits.)
#' * Sets the root directory to this directory
#' * Sets up the main chunk hook `litr::send_to_package()` that sends code to the 
#' R package directory.
#' * In the case that `minimal_eval=TRUE`, sets up an options hook for `eval` so
#'   chunks are only evaluated if there is a `usethis` or `litr::document()`
#'   command
#' * Deactivates an internal function of the `usethis` package
#' * Redefines the document output hook to handle chunk references differently  
#' * Sets up a [custom language engine](https://bookdown.org/yihui/rmarkdown-cookbook/custom-engine.html) called
#' `package_doc` that creates a package documentation file and then inserts
#' whatever the user puts in the chunk.
#' 
#' Returns the original state of the knitr objects that have been modified in 
#' setup.  This allows us to return things to the previous state after we are
#' finished.  This is relevant in the case where litr-knitting occurs in the 
#' current session and we don't want to leave things in a permanently modified
#' state.
#' 
#' @param package_dir Directory where R package will be created
#' @param minimal_eval If `TRUE`, then only chunks with `litr::document()` or 
#' `usethis` commands will be evaluated.  This can be convenient in coding when 
#' you just want to quickly update the R package without having to wait for long
#' evaluations to occur.
#' @keywords internal
setup <- function(package_dir, minimal_eval) {
  if (file.exists(package_dir)) {
    unedited <- tryCatch(check_unedited(package_dir),
                         error = function(e) {
                           # contents of package_dir does not resemble
                           # a litr package
                           return(FALSE)
                         })
    if (!unedited) {
      stop(make_noticeable(paste(
        stringr::str_glue("The directory {normalizePath(package_dir)}"),
        "already exists and either was not created by litr or may have manual",
        "edits. In either case, please rename that directory (or delete it)", 
        "and then try again.", 
        sep = "\n")))
    }
    unlink(package_dir, recursive = TRUE)
  }
  fs::dir_create(package_dir)
  usethis:::proj_set_(usethis:::proj_path_prep(package_dir))

  # let's keep a version of the knitr objects before modifying them:
  original_knitr <- list(opts_knit = knitr::opts_knit$get(),
                         knit_hooks = knitr::knit_hooks$get(),
                         opts_chunk = knitr::opts_chunk$get(),
                         opts_hooks = knitr::opts_hooks$get(),
                         knit_engines = knitr::knit_engines$get()
                         )
  
  knitr::opts_knit$set(root.dir = package_dir) # sets wd of future chunks
  knitr::knit_hooks$set(send_to_package = send_to_package)
  knitr::opts_chunk$set(send_to_package = TRUE)
  if (minimal_eval) {
    # only evaluate chunks that appear to include usethis commands or 
    # a call to litr::document() but if someone has specifically set eval=FALSE
    # in a particular chunk, do honor that
    usethis_exports <- getNamespaceExports("usethis")
    patterns <- paste(c("usethis::", usethis_exports, "litr::document\\("), collapse = "|")
    knitr::opts_hooks$set(eval = function(options) {
      if (options$eval)
        options$eval <- any(stringr::str_detect(options$code, patterns))
      return(options)
    })
  }
  
  
  # change usethis:::challenge_nested_project so that it will not complain
  # about creating a nested project (e.g. if this is called within a git 
  # subdirectory)
  utils::assignInNamespace("challenge_nested_project", function(...) NULL, ns = "usethis")
  # change usethis:::use_src_example_script so that it will not cause an error
  utils::assignInNamespace("use_src_example_script", 
                           function(...) {
                             usethis::use_template("code.cpp",
                                                   fs::path("src", "code.cpp"))
                           }, ns = "usethis")
  
  # define document hook to handle chunk references:
  knitr::knit_hooks$set(document = function(x) {
    # get the indices of x corresponding to code chunks
    chunk_start <- "^(\n```+[a-zA-Z0-9_]+\n)"
    idx_block <- stringr::str_which(x, chunk_start)
    original_code <- knitr::knit_code$get()
    # We first get indices of skipped chunks in original_code list
    skipped_chunks <- which(sapply(original_code, function(x){
      return(isFALSE(attr(x, "chunk_opts")$echo) || isFALSE(attr(x, "chunk_opts")$include))
    }))

    # Next we remove the indices of skipped chunks
    original_code_idx_fixed <- setdiff(seq(length(original_code)), skipped_chunks)
    
    labels <- names(original_code)
    # replace each x[i] that has code in it with the original code
    for (i in seq_along(idx_block)) {
      # break code into multiple lines:
      chunk <- strsplit(x[idx_block[i]], "\n")[[1]]
      # get the fence used (in case it's more than three ticks):
      i_start <- stringr::str_which(chunk, "^```+[a-zA-Z0-9_]+")
      fence <- stringr::str_replace(chunk[i_start[1]],
                                    "^(```+)[a-zA-Z0-9_]+", "\\1")
      i_fences <- stringr::str_which(chunk, paste0("^", fence))
      # there can be multiple code and output chunks strung together 
      # within a single x[i] if results are not held to end
      i_all_code <- c()
      for (j in seq_along(i_start)) {
        # get the elements corresponding the j-th code chunk within chunk
        i_code_end <- i_fences[which(i_fences == i_start[j]) + 1]
        i_all_code <- c(i_all_code, i_start[j]:i_code_end)
      }
      i_all_code <- setdiff(i_all_code, i_start[1])
      chunk_no_code <- chunk[-i_all_code]
      chunk <- c(chunk_no_code[1:i_start[1]],
                 original_code[original_code_idx_fixed[i]][[1]],
                 # insert the original version, accounting for skipped chunks
                 fence)
      if (i_start[1] < length(chunk_no_code))
        chunk <- c(chunk, chunk_no_code[(i_start[1] + 1):length(chunk_no_code)])
        x[idx_block[i]] <- paste(chunk, collapse = "\n")
    }
    
    # replace code chunks with the original code
    # (so we'll still have <<label>> chunk references)
    refs <- c() # labels that get referred to
    for (label in labels) {
      refs <- c(refs, find_labels(original_code[[label]])$chunk_ids)
    }
    refs <- unique(refs)
    adj_labels <- labels[!labels %in% names(skipped_chunks)]
    ref_id <- match(refs, adj_labels)
    to_insert <- paste0('###"', adj_labels[ref_id], '"###\n')
    x[idx_block[ref_id]] <- stringr::str_replace(x[idx_block[ref_id]],
                                                 chunk_start,
                                                 paste0("\\1", to_insert))
    x
  })
  
  # setup package_doc engine
  knitr::knit_engines$set(package_doc = function(options) {
    # create package_doc
    usethis::use_package_doc(open = FALSE)
    
    # insert the contents of the code chunk into the package_doc
    pkgdoc <- file.path("R", paste0(fs::path_file(package_dir), "-package.R"))
    add_text_to_file(options$code, filename = pkgdoc, location = 1)
    
    # now treat this as if it were standard R code with eval=FALSE
    r_engine <- knitr::knit_engines$get("R")
    options[["eval"]] <- FALSE
    return(r_engine(options))
  })
  return(original_knitr)
}

#' Make error messages noticeable
#' 
#' Since litr error messages are amid a lot of output from knitting, we'd like 
#' the litr ones to be eye-catching.
#' 
#' @param msg Error message
#' @keywords internal
make_noticeable <- function(msg) {
  paste("",
        "======",
        "Please read your friendly litr error message here:",
        paste("> ", msg),
        "======",
        sep = "\n")
}

#' A knitr chunk hook for writing R code and tests
#' 
#' This chunk hook detects whether a chunk is defining a function or dataset
#' to be included in the R package (looks for the `roxygen2` comment format `#' `).
#' If so, then it is written to the `R/` directory.  It also looks for chunks 
#' that have one or more lines that start with `test_that(` or 
#' `testthat::test_that(` (potentially with some leading whitespace).  These 
#' chunks are then written to the `tests` directory of the R package.
#' 
#' When the `send_to` option is used, this chunk hook instead simply writes the
#' code chunk to the file specified.
#' 
#' @param before Indicates whether this is being called before or after the 
#' chunk code is executed
#' @param options Has information from the chunk
#' @param envir Environment
#' @keywords internal
send_to_package <- function(before, options, envir) {
  msg <- do_not_edit_message(knitr::current_input(), type = "R")
  if (before == FALSE) {
    # Don't do anything after the code chunk has been executed.
    return()
  }
  package_dir <- knitr::opts_knit$get("root.dir")
  package_name <- fs::path_file(package_dir)
  if (!is.null(options$send_to)) {
    # the user has defined an option that indicates where in the package this
    # code should be written
    file <- file.path(package_dir, options$send_to)
    add_text_to_file(options$code, file, pad = TRUE)
    return()
  }
  if (stringr::str_detect(options$code[1], "^#' ")) {
    # starts with roxygen2, so let's assume this chunk is defining an R function
    # or dataset that belongs in the package
    non_comment <- stringr::str_subset(options$code, "^#", negate = TRUE)
    if (length(non_comment) > 0) {
      if (stringr::str_detect(non_comment[1], "<-")) {
        # a function is being defined
        objname <- stringr::str_match(non_comment[1], "^(.*)\\s*<-\\s*function")[, 2]
        objname <- stringr::str_trim(objname)
      } else if (stringr::str_detect(non_comment[1], '^".+"$')) {
        # a dataset is being documented
        objname <- stringr::str_sub(non_comment[1], start = 2, end = -2)
      } else {
        # Roxygen2 comment wasn't followed by anything recognized, so do not 
        # send this to package
        return()
      }
      file <- file.path(package_dir, "R", stringr::str_glue("{objname}.R"))
      cat(paste(c(msg, "", options$code, ""), collapse = "\n"), file = file)
    }
  }
  else if (any(stringr::str_detect(options$code,
                                   "^\\s*(testthat::)?test_that\\("))) {
    # This chunk is inferred to be a test
    test_dir <- file.path(package_dir, "tests", "testthat")
    test_file <- file.path(test_dir, "tests.R")
    if (!file.exists(test_file)) {
      # It's the first chunk with tests
      if (!dir.exists(test_dir)) usethis::use_testthat()
      cat(c(msg, ""), collapse = "\n", file = test_file)
    }
    cat(
      paste(c(options$code, "", ""), collapse = "\n"),
      file = test_file,
      append = TRUE
    )
  } else if (options$engine == "Rcpp") {
    # To add Rcpp code, we need the package documentation file to exist 
    if (!file.exists(file.path(
      package_dir,
      "R",
      paste0(package_name, "-package.R"))
      )) {
      usethis::use_package_doc(open = FALSE)
    }
    cpp_file <- file.path(package_dir, "src", "code.cpp")
    if (!file.exists(cpp_file)) {
      # set up package for Rcpp
      usethis::use_rcpp(name = "code")
      msg <- do_not_edit_message(knitr::current_input(), type = "c")
      cat(msg, file = cpp_file, append = TRUE)
    }
    # append code to code.cpp, but remove lines that are `#include <Rcpp.h>`
    # or `using namespace Rcpp;` since this already appears at top of file
    cat(paste(c(
      "",
      stringr::str_subset(
        options$code,
        r"(^#include <Rcpp.h>$|^using namespace Rcpp;$)",
        negate = TRUE),
      ""), collapse = "\n"), 
        file = cpp_file,
        append = TRUE)
  }
  return()
}

#' Add Some Text to a File
#' 
#' The text will be added to the file at a particular line specified by
#' `location`.  The first line of `txt` will be on line `location` of the
#' modified file.  If `location` is NULL, then text is added to end of file.
#' If file does not exist, it is created and `location` is ignored (unless 
#' `req_exist` is `TRUE`, in which case an error is thrown).
#' 
#' @param txt Character vector to add to file
#' @param filename Name of file
#' @param location Specifies where text should be added. See description for more.
#' @param req_exist If TRUE, then throws an error if file doesn't exist
#' @param pad If TRUE, then when text is being added to a preexisting file, it adds a newline
#' @keywords internal
add_text_to_file <- function(txt, filename, location = NULL, req_exist = FALSE,
                             pad = FALSE) {
  if (!file.exists(filename)) {
    if (req_exist) stop(stringr::str_glue("Cannot find file {filename}."))
    writeLines(txt, con = filename)
    return()
  }
  if (pad) txt <- c("", txt)
  filetxt <- readLines(filename)
  if (is.null(location) || location == length(filetxt) + 1) {
    filetxt <- c(filetxt, txt)
  }
  else if (location > length(filetxt) + 1 | location < 1) 
    stop("Invalid location")
  else if (location == 1) {
    filetxt <- c(txt, filetxt)
  } else {
    # location is somewhere in middle
    filetxt <- c(filetxt[1:(location - 1)],
                 txt,
                 filetxt[location:length(filetxt)])
  }
  writeLines(filetxt, con = filename)
}

#' Find a .Rmd chunk label in a code chunk
#' 
#' @param chunk_code Character vector of code from a .Rmd code chunk. Each element is a line of the code chunk.
#' @return List where chunk_idx is a logical vector for each line of the chunk corresponding to whether a chunk label of the form `<<label>>` was found and chunk_ids is a character vector of chunk label was found in that chunk.
#' @keywords internal
find_labels <- function(chunk_code) {
  rc <- knitr::all_patterns$md$ref.chunk
  chunk_idx <- any(idx = grepl(rc, chunk_code))
  chunk_ids <- stringr::str_trim(sub(rc, "\\1", chunk_code[grepl(rc, chunk_code)]))
  return(list(chunk_idx = chunk_idx, chunk_ids = chunk_ids))
}
