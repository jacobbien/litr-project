# Generated from _main.Rmd: do not edit by hand

build_all_templates <- function(example_dir) {
  if (fs::dir_exists(example_dir)) fs::dir_delete(example_dir)
  fs::dir_create(example_dir)
  # we knit from within `example_dir` and move things around afterwards, so we
  # want a path that does not depend on the working directory:
  example_dir <- fs::path_abs(example_dir)

  # map each template to its `draft_*()` function
  drafters <- list(
    "make-an-r-package" = litr::draft,
    "make-an-r-package-with-data" = litr::draft_data,
    "make-an-r-package-with-rcpp" = litr::draft_rcpp,
    "make-an-r-package-with-extras" = litr::draft_extras,
    "make-an-r-package-with-armadillo" = litr::draft_armadillo,
    "make-an-r-package-from-bookdown" = litr::draft_bookdown
    )
  # the templates of the installed `litr`, which is what the drafters read:
  templates <- fs::path_file(
    fs::dir_ls(system.file("rmarkdown", "templates", package = "litr"))
    )
  undrafted <- setdiff(templates, names(drafters))
  if (length(undrafted) > 0)
    stop(stringr::str_glue(
      "No draft function is known for the template(s) ",
      "{paste(undrafted, collapse = ', ')}."
      ))
  # the package name each drafter creates, e.g. "rhello":
  pkg_names <- lapply(drafters, function(drafter) {
    as.character(formals(drafter)$pkg_name)
    })

  templates_bookdown <- stringr::str_subset(templates, "bookdown")
  templates_nonbookdown <- setdiff(templates, templates_bookdown)
  for (template in templates_nonbookdown) {
    pkg_name <- pkg_names[[template]]
    drafters[[template]](dir = example_dir)
    rmd_file <- file.path(example_dir, paste0("create-", pkg_name, ".Rmd"))
    litr::render(rmd_file)
    # move to a template-specific directory:
    template_dir <- file.path(example_dir, template)
    fs::dir_create(template_dir)
    fs::file_move(fs::dir_ls(example_dir, regexp = pkg_name), template_dir)
    # move the source-files directory
    if (fs::dir_exists(file.path(example_dir, "source-files"))) {
      fs::dir_create(file.path(example_dir, template, "source-files"))
      fs::dir_copy(file.path(example_dir, "source-files"),
                   file.path(example_dir, template))
      fs::dir_delete(file.path(example_dir, "source-files"))
    }
    # move the docs directory (when pkgdown creates one)
    if (fs::dir_exists(file.path(example_dir, "docs"))) {
      fs::dir_create(file.path(example_dir, template, "docs"))
      fs::dir_copy(file.path(example_dir, "docs"),
                   file.path(example_dir, template))
      fs::dir_delete(file.path(example_dir, "docs"))
    }
  }

  for (template in templates_bookdown) {
    pkg_name <- pkg_names[[template]]
    drafters[[template]](dir = example_dir)
    # this drafter makes a directory rather than a single file:
    tmp_dir <- file.path(example_dir, paste0("create-", pkg_name))
    withr::with_dir(tmp_dir, litr::render("index.Rmd"))
    # move to a template-specific directory:
    fs::dir_copy(tmp_dir, file.path(example_dir, template))
    fs::dir_delete(tmp_dir)
  }
  invisible(example_dir)
}
