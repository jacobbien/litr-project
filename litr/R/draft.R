# Generated from _main.Rmd: do not edit by hand

#' Internal function for creating a .Rmd file from template
#' @param pkg_name Name of package to be created.
#' @param dir (Optional) Directory where .Rmd file should be created
#' @param template_name Name of template
#' @keywords internal
create_from_template <- function(pkg_name, dir, template_name) {
  filename <- stringr::str_glue(file.path(dir, "create-{pkg_name}.Rmd"))
  rmarkdown::draft(filename, template = template_name, package = "litr",
                   edit = FALSE)
  rmd <- readLines(filename)
  rmd <- stringr::str_replace(rmd, 'package_name: .+$',
                              stringr::str_glue('package_name: "{pkg_name}"'))
  writeLines(rmd, filename)
  message(stringr::str_glue("Created new file {filename}."))
}

#' Create a new litr .Rmd document for creating an R package
#' 
#' This creates `create-[pkg_name].Rmd` that when knitted (i.e., when passed to
#' `litr::render()`) will create an R package called `pkg_name`.
#' 
#' This is the most basic R package template, with one function and one test 
#' function.
#' @param pkg_name Name of package to be created.
#' @param dir (Optional) Directory where .Rmd file should be created
#' @export
#' @seealso \code{\link{draft_bookdown}} \code{\link{draft_data}} \code{\link{draft_rcpp}} \code{\link{draft_extras}} \code{\link{draft_armadillo}}
draft <- function(pkg_name = "rhello", dir = ".") {
  create_from_template(pkg_name, dir, "make-an-r-package")
}

#' Create a new litr .Rmd document for creating an R package with a data set
#' 
#' This creates `create-[pkg_name].Rmd` that when knitted (i.e., when passed to
#' `litr::render()`) will create an R package called `pkg_name`.
#' This template shows how to make an R package with a data set in it.
#' 
#' @param pkg_name Name of package to be created.
#' @param dir (Optional) Directory where .Rmd file should be created
#' @export
#' @seealso \code{\link{draft}} \code{\link{draft_bookdown}} \code{\link{draft_rcpp}} \code{\link{draft_extras}} \code{\link{draft_armadillo}}
draft_data <- function(pkg_name = "rhasdata", dir = ".") {
  create_from_template(pkg_name, dir, "make-an-r-package-with-data")
}

#' Create a new litr .Rmd document for creating an R package that uses `Rcpp`
#' 
#' This creates `create-[pkg_name].Rmd` that when knitted (i.e., when passed to
#' `litr::render()`) will create an R package called `pkg_name`.
#' This template shows how to make an R package that uses `Rcpp`.
#' 
#' @param pkg_name Name of package to be created.
#' @param dir (Optional) Directory where .Rmd file should be created
#' @export
#' @seealso \code{\link{draft}} \code{\link{draft_bookdown}} \code{\link{draft_data}} \code{\link{draft_extras}} \code{\link{draft_armadillo}}
draft_rcpp <- function(pkg_name = "withrcpp", dir = ".") {
  create_from_template(pkg_name, dir, "make-an-r-package-with-rcpp")
}

#' Create a new litr .Rmd document for creating an R package with extras
#' 
#' This creates `create-[pkg_name].Rmd` that when knitted (i.e., when passed to
#' `litr::render()`) will create an R package called `pkg_name`.
#' This template shows how to make an R package that has "extras" such as a
#' README, vignette(s), a pkgdown site, and a hex sticker.
#' 
#' @param pkg_name Name of package to be created.
#' @param dir (Optional) Directory where .Rmd file should be created
#' @export
#' @seealso \code{\link{draft}} \code{\link{draft_bookdown}} \code{\link{draft_data}} \code{\link{draft_rcpp}} \code{\link{draft_armadillo}}
draft_extras <- function(pkg_name = "withpkgdown", dir = ".") {
  create_from_template(pkg_name, dir, "make-an-r-package-with-extras")
}

#' Create a new litr .Rmd document for creating an R package that uses `RcppArmadillo`
#' 
#' This creates `create-[pkg_name].Rmd` that when knitted (i.e., when passed to
#' `litr::render()`) will create an R package called `pkg_name`.
#' This template shows how to make an R package that uses `RcppArmadillo`.
#' 
#' @param pkg_name Name of package to be created.
#' @param dir (Optional) Directory where .Rmd file should be created
#' @export
#' @seealso \code{\link{draft}} \code{\link{draft_bookdown}} \code{\link{draft_data}} \code{\link{draft_rcpp}} \code{\link{draft_extras}}
draft_armadillo <- function(pkg_name = "witharmadillo", dir = ".") {
  create_from_template(pkg_name, dir, "make-an-r-package-with-armadillo")
}

#' Create a new litr .Rmd document for creating an R package from `bookdown`
#' 
#' This template shows how to make an R package from `bookdown`. It creates a directory called `create-[pkg_name]` in `dir`.  Rendering the file `index.Rmd`
#' with `litr::render()` creates the bookdown and package.
#' 
#' @param pkg_name Name of package to be created.
#' @param dir (Optional) Directory where .Rmd file should be created
#' @export
#' @seealso \code{\link{draft}} \code{\link{draft_data}} \code{\link{draft_rcpp}} \code{\link{draft_extras}} \code{\link{draft_armadillo}}
draft_bookdown <- function(pkg_name = "frombookdown", dir = ".") {
  create_pkg <- stringr::str_glue("create-{pkg_name}")
  dirname <- stringr::str_glue(file.path(dir, create_pkg))
  rmarkdown::draft(dirname,
                   template = "make-an-r-package-from-bookdown",
                   package = "litr",
                   edit = FALSE)
  # delete unneeded file:
  fs::file_delete(file.path(dirname, paste0(create_pkg, ".Rmd")))
  index_file <- file.path(dirname, "index.Rmd")
  # adjust index.Rmd with name of package
  rmd <- readLines(index_file)
  rmd <- stringr::str_replace(rmd, 'package_name: .+$',
                              stringr::str_glue('package_name: "{pkg_name}"'))
  writeLines(rmd, index_file)
  message(stringr::str_glue("Created new directory {dirname}"))
}
