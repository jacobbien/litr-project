test_that("get_params_used works", {
  dir <- tempfile()
  if (fs::file_exists(dir)) fs::file_delete(dir)
  fs::dir_create(dir)
  rmd_file <- file.path(dir, "my-package.Rmd")
  rmarkdown::draft(rmd_file, template = "make-an-r-package", package = "litr",
                   edit = FALSE)
  default_params <- get_params_used(rmd_file, passed_params = list())
  expect_equal(
    default_params,
    rmarkdown::yaml_front_matter(rmd_file)$params
    )
  params1 <- default_params
  params1$package_parent_dir <- "dir"
  expect_equal(
    get_params_used(rmd_file, passed_params = list(package_parent_dir = "dir")),
    params1
  )
  params2 <- default_params
  params2$package_name <- "pkg"
  params2$package_parent_dir <- "dir"
  expect_equal(
    get_params_used(rmd_file, 
                    passed_params = list(package_parent_dir = "dir",
                                         package_name = "pkg")),
    params2
  )
  fs::dir_delete(dir)
})
