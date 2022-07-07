#' Literate Programming for Writing R Packages
#'
#' Allows one to fully create an R package in a single .Rmd file.  Includes
#' functionality and .Rmd templates for a literate programming approach to R
#' package development.
#' 
#' @examples
#' # Make a file create-rhello.Rmd based on a template
#' rmarkdown::draft("create-rhello.Rmd", 
#'                  template = "make-an-r-package",
#'                  package = "litr",
#'                  edit = FALSE)
#' # Now call litr::render (or press Knit if in RStudio) to generate not just
#' # create-rhello.html, but also an R package called `rhello`.
#' litr::render("create-rhello.Rmd")
#' @docType package
#' @seealso \code{\link{render}}
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL