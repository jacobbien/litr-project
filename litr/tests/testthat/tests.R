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

testthat::test_that("get_package_directory() works", {
  input <- file.path("inputdir", "input.Rmd")
  testthat::expect_equal(
    get_package_directory(".", "mypkg", input),
    file.path("inputdir", "mypkg") # inputdir/mypkg
  )
  testthat::expect_equal(
    get_package_directory("..", "mypkg", input),
    file.path("inputdir", "..", "mypkg") # inputdir/../mypkg
  )
})

testthat::test_that('load_all() works', {
  # setup files for tests:
  dir <- tempfile()
  if (fs::file_exists(dir)) fs::file_delete(dir)
  fs::dir_create(dir)
  rmd_file <- file.path(dir, 'create-pkg.Rmd')
  fs::file_copy(testthat::test_path("create-pkg.Rmd"), rmd_file)
  html_file <- file.path(dir, "create-pkg.html")

  load_all(rmd_file)
  testthat::expect_equal(say_hello("Jacob"), "Hello Jacob!")
  
  fs::dir_delete(dir)
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

testthat::test_that('Knuth-style references work', {
  dir <- tempfile()
  if (fs::file_exists(dir)) fs::file_delete(dir)
  fs::dir_create(dir)
  rmd_file <- file.path(dir, 'create-rknuth.Rmd')
  fs::file_copy(path = testthat::test_path("create-rknuth.Rmd"), new_path = rmd_file)
  render(rmd_file)
  testthat::expect_true(fs::file_exists(file.path(dir, 'create-rknuth.html')))
  fs::dir_delete(dir)
})

testthat::test_that('Rendering in all possible ways works', {
  
  # setup files for tests:
  dir <- tempfile()
  if (fs::file_exists(dir)) fs::file_delete(dir)
  fs::dir_create(dir)
  # .Rmd without output format in preamble
  rmd_file1 <- file.path(dir, 'create-pkg1.Rmd')
  fs::file_copy(testthat::test_path("create-pkg.Rmd"), rmd_file1)
  # .Rmd without output format in preamble
  rmd_file2 <- file.path(dir, 'create-pkg2.Rmd')
  fs::file_copy(rmd_file1, rmd_file2)
  litr:::add_text_to_file("output: litr::litr_html_document", rmd_file2, 3)
  # files names
  rmd_file <- file.path(dir, "create-pkg.Rmd")  
  html_file <- file.path(dir, "create-pkg.html")
  html_file_a <- file.path(dir, "a","create-pkg.html")
  pkg <- file.path(dir, "pkg")
  pkg_a <- file.path(dir, "a", "pkg")
  check_outputs_are_same <- function() {
    # html files should be the same:
    testthat::expect_equal(readLines(html_file_a), readLines(html_file))
    # packages should be the same (relying here on litr-hash in DESCRIPTION):
    testthat::expect_equal(readLines(file.path(pkg, "DESCRIPTION")),
                           readLines(file.path(pkg_a, "DESCRIPTION")))
  }

  ## Now test that all the cases give the same outputs:
  
  # Case 1: no preamble + litr::render()
  fs::file_copy(rmd_file1, rmd_file, overwrite = TRUE)
  render(rmd_file, output_file = html_file)
  if (fs::file_exists(file.path(dir, "a"))) fs::file_delete(file.path(dir, "a"))
  fs::dir_create(file.path(dir, "a"))
  fs::dir_copy(pkg, pkg_a)
  fs::dir_delete(pkg)
  fs::file_move(html_file, html_file_a)

  # Case 2: with preamble + litr::render()
  fs::file_copy(rmd_file2, rmd_file, overwrite = TRUE)
  render(rmd_file, output_file = html_file)
  check_outputs_are_same()
  
  # Case 3: no preamble + litr::render() with output format argument
  fs::file_copy(rmd_file1, rmd_file, overwrite = TRUE)
  render(rmd_file, output_format = litr::litr_html_document(),
         output_file = html_file)
  check_outputs_are_same()
  
  # Case 4: with preamble + litr::render() with output format argument
  fs::file_copy(rmd_file2, rmd_file, overwrite = TRUE)
  render(rmd_file, output_format = litr::litr_html_document(),
         output_file = html_file)
  check_outputs_are_same()

  # Case 5: with preamble + rmarkdown::render()
  fs::file_copy(rmd_file2, rmd_file, overwrite = TRUE)
  xfun::Rscript_call(rmarkdown::render,
                     list(input = rmd_file, output_file = html_file)
                     )
  check_outputs_are_same()

  # Case 6: no preamble + rmarkdown::render() with output format argument
  fs::file_copy(rmd_file1, rmd_file, overwrite = TRUE)
  xfun::Rscript_call(rmarkdown::render,
                     list(input = rmd_file,
                          output_format = litr::litr_html_document(),
                          output_file = html_file)
                     )
  check_outputs_are_same()

  # Case 7: with preamble + rmarkdown::render() with output format argument
  fs::file_copy(rmd_file2, rmd_file, overwrite = TRUE)
  xfun::Rscript_call(rmarkdown::render,
                     list(input = rmd_file,
                          output_format = litr::litr_html_document(),
                          output_file = html_file)
                     )
  check_outputs_are_same()
  
  fs::dir_delete(dir)
})

testthat::test_that('Rendering with minimal_eval=TRUE works', {
  
  # setup files for tests:
  dir <- tempfile()
  if (fs::file_exists(dir)) fs::file_delete(dir)
  fs::dir_create(dir)
  rmd_file <- file.path(dir, 'create-pkg.Rmd')
  fs::file_copy(testthat::test_path("create-pkg.Rmd"), rmd_file)
  # .Rmd without output format in preamble
  html_file <- file.path(dir, "create-pkg.html")
  html_file_a <- file.path(dir, "a","create-pkg.html")
  pkg <- file.path(dir, "pkg")
  pkg_a <- file.path(dir, "a", "pkg")

  ## Now test that all the cases give the same outputs:
  
  # Case 1: minimal_eval = FALSE
  render(rmd_file, output_file = html_file, minimal_eval = FALSE)
  if (fs::file_exists(file.path(dir, "a"))) fs::file_delete(file.path(dir, "a"))
  fs::dir_create(file.path(dir, "a"))
  fs::dir_copy(pkg, pkg_a)
  fs::dir_delete(pkg)

  # Case 2: minimal_eval = TRUE passed to render
  render(rmd_file, output_file = html_file, minimal_eval = TRUE)
  testthat::expect_equal(readLines(file.path(pkg, "DESCRIPTION")),
                         readLines(file.path(pkg_a, "DESCRIPTION")))

  # Case 3: minimal_eval = TRUE passed to output format
  render(rmd_file,
         output_file = html_file,
         output_format = litr::litr_html_document(minimal_eval = TRUE)
         )
  testthat::expect_equal(readLines(file.path(pkg, "DESCRIPTION")),
                         readLines(file.path(pkg_a, "DESCRIPTION")))

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
  
  rmd_file <- file.path(dir, "create-withpkgdown.Rmd")
  rmarkdown::draft(rmd_file,
                   template = "make-an-r-package-with-extras",
                   package = "litr",
                   edit = FALSE)
  render(rmd_file)
  testthat::expect_true(fs::file_exists(file.path(dir, "create-withpkgdown.html")))
  testthat::expect_true(fs::file_exists(file.path(dir, "withpkgdown")))

  rmd_file <- file.path(dir, "create-frombookdown.Rmd")
  rmarkdown::draft(rmd_file,
                   template = "make-an-r-package-from-bookdown",
                   package = "litr",
                   edit = FALSE)
  prev_dir <- getwd()
  setwd(file.path(dir, "create-frombookdown"))
  fs::file_delete("create-frombookdown.Rmd")
  render("index.Rmd")
  setwd(prev_dir)
  testthat::expect_true(
    fs::file_exists(file.path(dir, "create-frombookdown", "_book", "index.html"))
    )
  testthat::expect_true(
    fs::file_exists(file.path(dir, "create-frombookdown", "frombookdown"))
    )

  fs::dir_delete(dir)
 })

