# Generated from create-litr.Rmd: do not edit by hand

#' Add README to package
#' 
#' This function takes a README.Rmd file, copies it into the package, and then
#' renders it to a README.md file.  It also adds these two files to the
#' .Rbuildignore.
#' 
#' @param rmd_file The path to a .Rmd file.
#' @export
add_readme <- function(rmd_file) {
  usethis::use_readme_rmd(open = FALSE)
  fs::file_copy(rmd_file, new_path = "README.Rmd", overwrite = TRUE)
  out <- xfun::Rscript_call(
    rmarkdown::render,
    args = list(
      input = "README.Rmd",
      output_options = list(html_preview = "false")
    )
  )
}

#' Add a hex sticker to package
#' 
#' In addition to calling this function, you should add to your README.Rmd something like this:
#' 
#' `# your-title <img src="man/figures/logo.png" align="right" height="139" />`
#' 
#' See [here](https://pkgdown.r-lib.org/reference/build_home.html#package-logo) 
#' for more.
#' 
#' @param hex_png_file The .png file with your package's hex sticker
#' @export
add_hex_sticker <- function(hex_png_file) {
  figures_dir <- file.path("man", "figures")
  fs::dir_create(figures_dir)
  fs::file_copy(path = hex_png_file, 
                new_path = file.path(figures_dir, "logo.png"), 
                overwrite = TRUE)
}

#' Add one or more vignettes to package
#' 
#' @param rmd_files A character vector of .Rmd files, each corresponding to 
#' a vignette
#' @param other_files A character vector of any other files needed in the 
#' vignettes directory (.bib file, images, etc.)
#' @export
add_vignettes <- function(rmd_files, other_files = NULL) {
  fs::dir_create("vignettes")
  for (fn in c(rmd_files, other_files)) fs::file_copy(fn, "vignettes")
  
  # update DESCRIPTION file:
  deps <- desc::desc_get_deps()$package
  if (!("knitr" %in% deps))
    desc::desc_set_dep("knitr", type = "Suggests")
  if (!("rmarkdown" %in% deps))
    desc::desc_set_dep("rmarkdown", type = "Suggests")
  out <- desc::desc_set("VignetteBuilder", "knitr")
}

#' Add a pkgdown site
#' 
#' This function creates a website for your package.  You can see it locally by
#' opening `docs/index.html` in your package.  To get it online you can copy the
#' `docs` directory to your website's server.
#' 
#' Be sure that in the generating .Rmd file this is called *after*
#' `litr::document()` has been called.  To customize the site, you may pass a
#' customized `_pkgdown.yml` file as described in [this `pkgdown` vignette](https://pkgdown.r-lib.org/articles/customise.html).
#' 
#' @param config_path The _pkgdown.yml file that lives somewhere outside of your package.  If NULL, then a basic default will be used.
#' @export
add_pkgdown <- function(config_path = NULL) {
  config_file <- "_pkgdown.yml"
  destdir <- "docs"
  usethis::use_build_ignore(c(config_file, destdir, "pkgdown"))
  if (is.null(config_path)) {
    # create a new config file (note it lives outside of package)
    config <- usethis:::pkgdown_config(destdir)
    usethis::write_over(config_file, yaml::as.yaml(config))
  } else {
    # copy the one that already exists:
    fs::file_copy(config_path, config_file)  
  }
  pkgdown::build_site()
}
