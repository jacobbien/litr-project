# Generated from create-litr.Rmd: do not edit by hand  
testthat::test_that("check_unedited works", {
  # Including this next line seems to be necessary for R CMD check on the cmd line:
  #Sys.setenv(RSTUDIO_PANDOC = "/Applications/RStudio.app/Contents/MacOS/pandoc")
  dir <- tempfile()
  fs::dir_create(dir)
  rmd_file <- file.path(dir, "my-package.Rmd")
  rmarkdown::draft(rmd_file, template = "make-an-r-package", package = "litr",
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
  i_litr <- stringr::str_which(txt, description_litr_field_name())
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

