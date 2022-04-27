test_that("check_unedited works", {
  # Including this next line seems to be necessary for R CMD check on the cmd line:
  #Sys.setenv(RSTUDIO_PANDOC = "/Applications/RStudio.app/Contents/MacOS/pandoc")

  dir <- tempfile()
  fs::dir_create(dir)
  rmd_path <- file.path(dir, "my-package.Rmd")
  rmarkdown::draft(rmd_path,
                   template = "make-an-r-package",
                   package = "litr",
                   edit = FALSE)
  # create R package (named "rhello") from the Rmd template:
  litr::render(rmd_path)
  package_path <- file.path(dir, "rhello")
  expect_true(litr::check_unedited(package_path))

  # what if a file has been added?
  added_file <- file.path(package_path, "R", "say_hello2.R")
  writeLines("# Added something here.", added_file)
  expect_false(litr::check_unedited(package_path))

  # what if we now remove it?
  fs::file_delete(added_file)
  expect_true(litr::check_unedited(package_path))

  # what if a file is removed from package?
  rfile <- file.path(package_path, "R", "say_hello.R")
  fs::file_move(rfile, dir)
  expect_false(litr::check_unedited(package_path))
  # now put it back
  fs::file_move(file.path(dir, "say_hello.R"), file.path(package_path, "R"))
  expect_true(litr::check_unedited(package_path))

  # what if something is changed in a file?
  txt <- readLines(rfile)
  txt_mod <- txt
  txt_mod[3] <- paste0(txt[3], " # added a comment!!")
  writeLines(txt_mod, rfile)
  expect_false(litr::check_unedited(package_path))
  # now put it back
  writeLines(txt, rfile)
  expect_true(litr::check_unedited(package_path))

  # what if something is changed in the DESCRIPTION file?
  descfile <- file.path(package_path, "DESCRIPTION")
  txt <- readLines(descfile)
  txt_mod <- txt
  txt_mod[1] <- "Package: newname"
  writeLines(txt_mod, descfile)
  expect_false(litr::check_unedited(package_path))
  # now put it back
  writeLines(txt, descfile)
  expect_true(litr::check_unedited(package_path))

  # what if the special litr hash field is changed in the DESCRIPTION file?
  txt <- readLines(descfile)
  i_litr <- stringr::str_which(txt, litr:::description_litr_field_name())
  txt_mod <- txt
  txt_mod[i_litr] <- paste0(txt_mod[i_litr], "a")
  writeLines(txt_mod, descfile)
  expect_false(litr::check_unedited(package_path))
  # now put it back
  writeLines(txt, descfile)
  expect_true(litr::check_unedited(package_path))

  fs::dir_delete(dir)
})
