# Generated from create-litr.Rmd: do not edit by hand  
testthat::test_that("add_text_to_file() works", {
  dir <- tempfile()
  if (fs::file_exists(dir)) fs::file_delete(dir)
  fs::dir_create(dir)
  
  # should throw error when file does not exist and req_exist is TRUE:
  myfile <- file.path(dir, "file.txt")
  sometxt <- c("hello", "there")
  testthat::expect_error(add_text_to_file(sometxt, myfile, req_exist = TRUE))

  # should create a new file where one does not exist:
  myfile <- file.path(dir, "file.txt")
  sometxt <- c("hello", "there")
  add_text_to_file(sometxt, myfile)
  testthat::expect_true(fs::file_exists(myfile))
  testthat::expect_equal(sometxt, readLines(myfile))
  
  # should append to end of file by default
  moretxt <- "world"
  add_text_to_file(moretxt, myfile)
  testthat::expect_equal(c(sometxt, moretxt), readLines(myfile))
   
  # should throw error for invalid locations:
  testthat::expect_error(add_text_to_file(sometxt, myfile, 0))
  testthat::expect_error(add_text_to_file(sometxt, myfile, -1))
  testthat::expect_error(add_text_to_file(sometxt, myfile, 5))

  # should add to specified line:
  moretxt2 <- "hi"
  add_text_to_file(moretxt2, myfile, 1)
  testthat::expect_equal(c(moretxt2, sometxt, moretxt), readLines(myfile))

  # should add to specified line:
  moretxt3 <- "hi2"
  add_text_to_file(moretxt3, myfile, 2)
  testthat::expect_equal(c(moretxt2, moretxt3, sometxt, moretxt),
                         readLines(myfile))

  # should add to specified line:
  moretxt4 <- "hi3"
  add_text_to_file(moretxt4, myfile, 6)
  testthat::expect_equal(c(moretxt2, moretxt3, sometxt, moretxt, moretxt4),
                         readLines(myfile))
  fs::dir_delete(dir)
})

no_chunks <- unlist(strsplit("---\ntitle: `pre_process_chunk_labels` Example\nauthor: Me\ndate: 2022-05-27\noutput: html_document\n---\n", "\n"), recursive = FALSE)

no_reference <- unlist(strsplit("---\ntitle: `pre_process_chunk_labels` Example\nauthor: Me\ndate: 2022-05-27\noutput: html_document\n---\n\n# Setup\n\n```{r setup, include = FALSE}\nknitr::opts_chunk$set(echo = TRUE)\n```\n\n```{r check-arg, eval=F}\n    if(!is.numeric(x)) stop('blah')\n```\n", "\n"), recursive = FALSE)

one_reference <- unlist(strsplit("```{r c2f}\nC2F <- function(x){\n    <<check-arg>>\n}\n```\n\n```{r check-arg, eval=F}\n    if(!is.numeric(x)) stop('blah')\n```\n", "\n"), recursive = FALSE)

missing_ref_chunk <- unlist(strsplit("```{r c2f}\nC2F <- function(x){\n    <<check-arg>>\n    <<convert-c2f>>\n}\n```\n\n```{r check-arg, eval=F}\n    if(!is.numeric(x)) stop('blah')\n```\n", "\n"), recursive = F)

chunk_with_no_label <- unlist(strsplit("---\ntitle: `pre_process_chunk_labels` Example\nauthor: Me\ndate: 2022-05-27\noutput: html_document\n---\n\n```{r}\n    1+2\n```\n", "\n"), recursive = FALSE)

testthat::test_that("preprocess_chunk_labels works as expected", {
  # handle the case where we are given a document with no codes chunks and when we do not have any chunk references
  # for both cases preprocess_chunk_labels should return the original rmd vector
  testthat::expect_equal(preprocess_chunk_labels(no_chunks), no_chunks)
  testthat::expect_equal(preprocess_chunk_labels(no_reference), no_reference)
  
  # check that when there is a chunk reference, we correctly change the delimiter, create a duplicate chunk and add a comment to referenced chunk
  testthat::expect_equal(preprocess_chunk_labels(one_reference), unlist(strsplit("```{r c2f, eval=FALSE}\nC2F <- function(x){\n    @@@check-arg@@@\n}\n```\n```{r c2f-dup, include=FALSE}\nC2F <- function(x){\n    <<check-arg>>\n}\n```\n\n```{r check-arg, eval=F}\n###\"check-arg\"###\n    if(!is.numeric(x)) stop('blah')\n```", "\n"), recursive = FALSE))
  
  # throw error if we can't find a referenced chunk in the file
  testthat::expect_error(preprocess_chunk_labels(missing_ref_chunk), "Unable to find the following chunk reference(s) in this Rmarkdown file: convert-c2f.", fixed=TRUE)
})

rmd_mini_example <- unlist(strsplit("---\ntitle: `pre_process_chunk_labels` Example\nauthor: Me\ndate: 2022-05-27\noutput: html_document\n---\n\n# Setup\n\n```{r setup, include = FALSE}\nknitr::opts_chunk$set(echo = TRUE)\n```\n\n# Content\n\n\n```{r c2f}\nC2F <- function(x){\n    <<check-arg>>\n    <<convert-c2f>>\n}\n```\n\n```{r check-arg, eval=F}\n    if(!is.numeric(x)) stop('blah')\n```\n", "\n"), recursive = FALSE)

testthat::test_that("extract_rmd_code_chunks works as expected", {
  # handle the case where we are given a document with no codes chunks
  # in the case of extract_rmd_code_chunks we should just return an empty list
  testthat::expect_equal(extract_rmd_code_chunks(no_chunks), list())
  # handle the case where we do have code chunks but no references
  # we expect that chunk_idx should be FALSE and no chunk_ids (thus length is 0)
  no_reference_chunks <- extract_rmd_code_chunks(no_reference)
  no_reference_chunk_idx <- sapply(no_reference_chunks, function(x){x$contains_labels$chunk_idx})
  no_reference_chunk_ids <- sapply(no_reference_chunks, function(x){length(x$contains_labels$chunk_ids)})
  testthat::expect_equal(no_reference_chunk_idx, c(FALSE, FALSE))
  testthat::expect_equal(no_reference_chunk_ids, c(0, 0))
  # we should pull all chunks from an Rmd file
  mini_example_chunks <- extract_rmd_code_chunks(rmd_mini_example)
  
  mini_example_chunk_idx <- sapply(mini_example_chunks, function(x){x$contains_labels$chunk_idx})
  mini_example_chunk_ids <- sapply(mini_example_chunks, function(x){length(x$contains_labels$chunk_ids)})
  mini_example_chunk_labels <- sapply(mini_example_chunks, function(x){x$chunk_label})
  
  testthat::expect_equal(mini_example_chunk_idx, c(FALSE, TRUE, FALSE))
  testthat::expect_equal(mini_example_chunk_ids, c(0, 2, 0))
  testthat::expect_equal(mini_example_chunk_labels, c("setup", "c2f", "check-arg"))
})

testthat::test_that("check_unedited works", {
  # Including this next line seems to be necessary for R CMD check on the cmd line:
  #Sys.setenv(RSTUDIO_PANDOC = "/Applications/RStudio.app/Contents/MacOS/pandoc")
  dir <- tempfile()
  fs::dir_create(dir)
  rmd_file <- file.path(dir, "my-package.Rmd")
  rmarkdown::draft(rmd_file,
                   template = "make-an-r-package",
                   package = "litr",
                   edit = FALSE)
  # create R package (named "rhello") from the Rmd template:
  render(rmd_file)
  package_path <- file.path(dir, "rhello")
  testthat::expect_true(check_unedited(package_path))

  # what if a file has been added?
  added_file <- file.path(package_path, "R", "say_hello2.R")
  writeLines("# Added something here.", added_file)
  testthat::expect_false(check_unedited(package_path))

  # what if we now remove it?
  fs::file_delete(added_file)
  testthat::expect_true(check_unedited(package_path))

  # what if a file is removed from package?
  rfile <- file.path(package_path, "R", "say_hello.R")
  fs::file_move(rfile, dir)
  testthat::expect_false(check_unedited(package_path))
  # now put it back
  fs::file_move(file.path(dir, "say_hello.R"), file.path(package_path, "R"))
  testthat::expect_true(check_unedited(package_path))

  # what if something is changed in a file?
  txt <- readLines(rfile)
  txt_mod <- txt
  txt_mod[3] <- paste0(txt[3], " # added a comment!!")
  writeLines(txt_mod, rfile)
  testthat::expect_false(check_unedited(package_path))
  # now put it back
  writeLines(txt, rfile)
  testthat::expect_true(check_unedited(package_path))

  # what if something is changed in the DESCRIPTION file?
  descfile <- file.path(package_path, "DESCRIPTION")
  txt <- readLines(descfile)
  txt_mod <- txt
  txt_mod[1] <- "Package: newname"
  writeLines(txt_mod, descfile)
  testthat::expect_false(check_unedited(package_path))
  # now put it back
  writeLines(txt, descfile)
  testthat::expect_true(check_unedited(package_path))

  # what if the special litr hash field is changed in the DESCRIPTION file?
  txt <- readLines(descfile)
  i_litr <- stringr::str_which(txt, description_litr_hash_field_name())
  txt_mod <- txt
  txt_mod[i_litr] <- paste0(txt_mod[i_litr], "a")
  writeLines(txt_mod, descfile)
  testthat::expect_false(check_unedited(package_path))
  # now put it back
  writeLines(txt, descfile)
  testthat::expect_true(check_unedited(package_path))

  fs::dir_delete(dir)
})

testthat::test_that("get_params_used works", {
  dir <- tempfile()
  if (fs::file_exists(dir)) fs::file_delete(dir)
  fs::dir_create(dir)
  rmd_file <- file.path(dir, "my-package.Rmd")
  rmarkdown::draft(rmd_file, template = "make-an-r-package", package = "litr",
                   edit = FALSE)
  default_params <- get_params_used(rmd_file, passed_params = list())
  testthat::expect_equal(
    default_params,
    rmarkdown::yaml_front_matter(rmd_file)$params
  )
  params1 <- default_params
  params1$package_parent_dir <- "dir"
  testthat::expect_equal(
    get_params_used(rmd_file, passed_params = list(package_parent_dir = "dir")),
    params1
  )
  params2 <- default_params
  params2$package_name <- "pkg"
  params2$package_parent_dir <- "dir"
  testthat::expect_equal(
    get_params_used(rmd_file,
                    passed_params = list(package_parent_dir = "dir",
                                         package_name = "pkg")),
    params2
  )
  fs::dir_delete(dir)
})

testthat::test_that("templates can be knit", {
  dir <- tempfile()
  if (fs::file_exists(dir)) fs::file_delete(dir)
  fs::dir_create(dir)
  
  rmd_file <- file.path(dir, "create-rhello.Rmd")
  rmarkdown::draft(rmd_file,
                   template = "make-an-r-package",
                   package = "litr",
                   edit = FALSE)
  render(rmd_file)
  testthat::expect_true(fs::file_exists(file.path(dir, "create-rhello.html")))
  testthat::expect_true(fs::file_exists(file.path(dir, "rhello")))

  rmd_file <- file.path(dir, "create-rhasdata.Rmd")
  rmarkdown::draft(rmd_file,
                   template = "make-an-r-package-with-data",
                   package = "litr",
                   edit = FALSE)
  render(rmd_file)
  testthat::expect_true(fs::file_exists(file.path(dir, "create-rhasdata.html")))
  testthat::expect_true(fs::file_exists(file.path(dir, "rhasdata")))

  rmd_file <- file.path(dir, "create-withrcpp.Rmd")
  rmarkdown::draft(rmd_file,
                   template = "make-an-r-package-with-rcpp",
                   package = "litr",
                   edit = FALSE)
  render(rmd_file)
  testthat::expect_true(fs::file_exists(file.path(dir, "create-withrcpp.html")))
  testthat::expect_true(fs::file_exists(file.path(dir, "withrcpp")))
  
  fs::dir_delete(dir)
 })

