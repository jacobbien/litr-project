# Generated from create-litr.Rmd: do not edit by hand

#' A knitr chunk hook for writing R code and tests
#' 
#' This chunk hook detects whether a chunk is defining a function or dataset
#' to be included in the R package (looks for the Roxygen2 comment format `#' `).
#' If so, then it is written to the `R/` directory.  It also looks for chunks 
#' with `testthat::` in them, which are written to the `tests` directory of the 
#' R package.
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
    # Also, don't do anything when processing the setup code chunk.
    return()
  }
  else if (stringr::str_detect(options$code[1], "^#' ")) {
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
      # check whether a specific location for the object has been set by the user
      obj_filename <- options$location
      if(is.null(obj_filename)){
        # send the object to {objname}.R
        file <- file.path(envir$package_dir, "R", stringr::str_glue("{objname}.R"))
        cat(paste(c(msg, "", options$code, ""), collapse = "\n"), file = file)
      } else{
        # the user specified a location to send the object
        file <- file.path(envir$package_dir, options$location)
        print(file)
        # check that the directory of the relative path given exists
        if(!fs::dir_exists(fs::path_dir(file))){
            stop(stringr::str_glue("directory {fs::path_dir(file)} does not exist"))
        }
        # give warning if the file name does not have .R extension
        if(length(fs::path_ext(file)) == 0){
            file <- fs::path_ext_set(file, "R")
            warning(stringr::str_glue("No file extension found in supplied file name, sending {objname} to {file}"))
        }
        # check whether a file with this name has already been created
        if(fs::file_exists(file)){
            # only append the code, don't add the message
            cat(paste(c("", options$code, ""), collapse = "\n"), file = file, append=TRUE)
        } else{
            # send the object to the user-supplied location with the message
            cat(paste(c(msg, "", options$code, ""), collapse = "\n"), file = file)
        }
        
      }
      
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

#' Code for setup chunk
#' 
#' Creates directory where package will be. (Deletes what is currently there as 
#' long as it appears to have been created by litr and does not have any 
#' subsequent manual edits.)  Sets the root directory to this directory and 
#' sets up the main chunk hook `litr::send_to_package` that sends code to the R 
#' package directory.
#' 
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
  knitr::opts_knit$set(root.dir = package_dir) # sets wd of future chunks
  knitr::knit_hooks$set(send_to_package = litr::send_to_package)
  knitr::opts_chunk$set(send_to_package = TRUE)
  # change usethis:::challenge_nested_project so that it will not complain
  # about creating a nested project (e.g. if this is called within a git 
  # subdirectory)
  utils::assignInNamespace("challenge_nested_project", function(...) NULL, ns = "usethis")
}

#' Make error messages noticeable
#' 
#' Since litr error messages are amid a lot of output from knitting, we'd like 
#' the litr ones to be eye-catching.
#' 
#' @param msg Error message
make_noticeable <- function(msg) {
  paste("",
        "======",
        "Please read your friendly litr error message here:",
        paste("> ", msg),
        "======",
        sep = "\n")
}
