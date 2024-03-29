% Generated by roxygen2: do not edit by hand
% Please edit documentation in _main.Rmd.
\name{test_litr}
\alias{test_litr}
\title{Run tests for \code{litr} itself}
\usage{
test_litr(install_old, location_of_new)
}
\arguments{
\item{install_old}{A function that when run will install the old version}

\item{location_of_new}{Path to the new package directory}
}
\description{
Special function for testing \code{litr}.  The trick is to temporarily install
the new version of \code{litr}, run the test, and then put things back how it was
before.
}
\details{
Typical values for \code{install_old} could be
\itemize{
\item \code{function() devtools::install("[location of old version]")}
\item \code{function() remotes::install_github("jacobbien/litr-project@*release", subdir = "litr")}.
}
}
\keyword{internal}