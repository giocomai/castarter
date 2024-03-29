% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cas_ia_check.R
\name{cas_ia_save}
\alias{cas_ia_save}
\title{Save a URL the Internet Archive's Wayback Machine}
\usage{
cas_ia_save(
  url = NULL,
  wait = 32,
  retry_times = 3,
  pause_base = 16,
  pause_cap = 1024,
  pause_min = 64,
  only_if_unavailable = TRUE,
  ia_check = TRUE,
  ia_check_wait = 2,
  db_connection = NULL,
  check_db = TRUE,
  write_db = TRUE,
  ...
)
}
\arguments{
\item{url}{A charachter vector of length one, a url.}

\item{wait}{Defaults to 32. I have found no information online about what
wait time is considered suitable by Archive.org itself, but I've noticed
that with wait time shorter than 10 seconds the whole process stops getting
positive replies from the server very soon.}

\item{retry_times}{Defaults to 3. Number of times to retry download in case
of errors.}

\item{pause_base, pause_cap}{This method uses exponential back-off with full
jitter - this means that each request will randomly wait between
\code{pause_min} and \code{pause_base * 2 ^ attempt} seconds, up to a maximum of
\code{pause_cap} seconds.}

\item{pause_min}{Minimum time to wait in the backoff; generally
only necessary if you need pauses less than one second (which may
not be kind to the server, use with caution!).}

\item{only_if_unavailable}{Defaults to TRUE. If TRUE, checks for availability
of urls before attempting to save them.}

\item{ia_check}{Defaults to TRUE. If TRUE, checks again the URL after saving
it and keeps record in the local database.}

\item{ia_check_wait}{Defaults to 2, passed to \code{cas_ia_check()}. Can generally
be kept low, as this is a light API.}

\item{check_db}{Defaults to TRUE. If TRUE, checks if given URL has already
been checked in local database, and queries APIs only for URLs that have
not been previously checked.}

\item{write_db}{Defaults to TRUE. If TRUE, writes result to a local database.}

\item{...}{Passed to \code{cas_get_db_file()}.}
}
\description{
Consider using long waiting times, and using a high number of retry. Retry is
done graciously, using \code{httr::RETRY}, and respecting the waiting time given
when error 529 "too many requests" is returned by the server. This is still
likely to take a long amount of time.
}
\examples{
\dontrun{
if (interactive()) {
  # Once the usual parameters are set with `cas_set_options()` it is generally
  # ok to just let it get urls from the database and let it run without any
  # additional parameter.
  cas_ia_save()
}
}
}
