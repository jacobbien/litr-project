% Generated by roxygen2: do not edit by hand
% Please edit documentation in _main.Rmd.
\name{add_pkgdown}
\alias{add_pkgdown}
\title{Add a pkgdown site}
\usage{
add_pkgdown(config_path = NULL)
}
\arguments{
\item{config_path}{The _pkgdown.yml file that lives somewhere outside of your package.  If NULL, then a basic default will be used.}
}
\description{
This function creates a website for your package.  You can see it locally by
opening \code{docs/index.html} in your package.  To get it online you can copy the
\code{docs} directory to your website's server.
}
\details{
Be sure that in the generating .Rmd file this is called \emph{after}
\code{litr::document()} has been called.  To customize the site, you may pass a
customized \verb{_pkgdown.yml} file as described in \href{https://pkgdown.r-lib.org/articles/customise.html}{this \code{pkgdown} vignette}.
}