% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cas_generate_metadata.R
\name{cas_generate_metadata}
\alias{cas_generate_metadata}
\title{Generate basic metadata about the corpus, including start and end date and
total number of items available.}
\usage{
cas_generate_metadata(
  corpus = NULL,
  db_connection = NULL,
  db_folder = NULL,
  ...
)
}
\arguments{
\item{...}{Passed to \code{cas_get_db_file()}.}
}
\value{
A list.
}
\description{
Generate basic metadata about the corpus, including start and end date and
total number of items available.
}
