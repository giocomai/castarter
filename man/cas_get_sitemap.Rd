% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cas_get_sitemap.R
\name{cas_get_sitemap}
\alias{cas_get_sitemap}
\title{Checks for availability of a sitemap in xml format.}
\usage{
cas_get_sitemap(domain = NULL, sitemap_url = NULL)
}
\arguments{
\item{domain}{Defaults to \code{NULL}, but required unless \code{sitemap_url} given.
Expected to be a full domain name. If input does not start with \code{http},
then \verb{https://} is prepended automatically.}

\item{sitemap_url}{Defaults to \code{NULL}. If given, \code{domain} is ignored.}
}
\value{
A data frame, including a \code{sitemap_url} column, the response as an
httr2 object, and the body of the xml.
}
\description{
Searches in common locations (namely, \code{example.com/sitemap.xml}, and
\code{example.com/sitemap_index.xml}) and then in robots.txt and returns a url to
the sitemap, along with the contents of the sitemap itself, if found.
}
\examples{
if (interactive()) {
  cas_get_sitemap("https://www.europeandatajournalism.eu/")
}
}
