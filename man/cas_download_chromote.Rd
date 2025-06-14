% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cas_download_chromote.R
\name{cas_download_chromote}
\alias{cas_download_chromote}
\title{Downloads one file at a time with \code{chromote}}
\usage{
cas_download_chromote(
  download_df = NULL,
  index = FALSE,
  index_group = NULL,
  overwrite_file = FALSE,
  ignore_id = TRUE,
  wait = 1,
  delay = 0,
  timeout = 20,
  db_connection = NULL,
  sample = FALSE,
  file_format = "html",
  download_again = FALSE,
  download_again_if_status_is_not = NULL,
  disconnect_db = FALSE,
  ...
)
}
\arguments{
\item{download_df}{A data frame with four columns: \code{id}, \code{url}, \code{path}, \code{type}.}

\item{index}{Logical, defaults to FALSE. If TRUE, downloaded files will be
considered \code{index} files. If not, they will be considered \code{contents} files.
See Readme for a more extensive explanation.}

\item{overwrite_file}{Logical, defaults to FALSE.}

\item{wait}{Defaults to 1. Number of seconds to wait between downloading one
page and the next. Can be increased to reduce server load, or can be set to
0 when this is not an issue.}

\item{delay}{Defaults to 0. Passed to \code{chromote}'s internal method \code{go_to}.
Number of seconds to wait after the page load event fires.}

\item{timeout}{Defaults to 20. Passed to \code{chromote}'s internal method
\code{go_to}. Maximum time in seconds to wait for the page load event.}

\item{db_connection}{Defaults to NULL. If NULL, uses local SQLite database.
If given, must be a connection object or a list with relevant connection
settings (see example).}

\item{sample}{Defaults to FALSE. If TRUE, the download order is randomised.
If a numeric is given, the download order is randomised and at most the
given number of items is downloaded.}

\item{file_format}{Defaults to \code{html}. Used for storing files in dedicated
folders, but also for determining processing options. For example, if a
sitemap is downloaded as an index with \code{file_format} set to xml, it will be
processed accordingly. If it is stored as xml.gz, it will be automatically
decompressed for correct processing.}

\item{download_again_if_status_is_not}{Defaults to NULL. If given, it must a
status code as integer, typically \code{200L}, or \code{c(200L, 404L)}.}

\item{disconnect_db}{Defaults to TRUE. If FALSE, leaves the connection to database open.}

\item{...}{Passed to \code{cas_get_db_file()}.}
}
\description{
Downloads one file at a time with \code{chromote}
}
