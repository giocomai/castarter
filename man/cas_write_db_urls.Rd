% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cas_write_db_urls.R
\name{cas_write_db_urls}
\alias{cas_write_db_urls}
\title{Write index or contents urls directly to the local database}
\usage{
cas_write_db_urls(
  urls,
  index = FALSE,
  overwrite = FALSE,
  db_connection = NULL,
  disconnect_db = FALSE,
  quiet = FALSE,
  check_previous = TRUE,
  ...
)
}
\arguments{
\item{urls}{A data frame with five columns, such as
\code{casdb_empty_contents_id}, or a character vector.}

\item{index}{Logical, defaults to FALSE. If TRUE, downloaded files will be
considered \code{index} files. If not, they will be considered \code{contents} files.
See Readme for a more extensive explanation.}

\item{overwrite}{Logical, defaults to FALSE. If TRUE, checks if matching data
are previously held in the table and overwrites them. This should be used
with caution, as it may overwrite completely the selected table.}

\item{db_connection}{Defaults to NULL. If NULL, uses local SQLite database.
If given, must be a connection object or a list with relevant connection
settings (see example).}

\item{disconnect_db}{Defaults to TRUE. If FALSE, leaves the connection to database open.}

\item{quiet}{Defaults to FALSE. If set to TRUE, messages on number of lines
added are not shown.}

\item{check_previous}{Defaults to TRUE. If set to FALSE, the given input is
stored in the database without checking if the same url had already been
stored.}

\item{...}{Passed to \code{cas_get_db_file()}.}
}
\description{
Write index or contents urls directly to the local database
}
