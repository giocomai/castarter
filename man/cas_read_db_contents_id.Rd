% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cas_db_contents_id.R
\name{cas_read_db_contents_id}
\alias{cas_read_db_contents_id}
\title{Read contents from local database}
\usage{
cas_read_db_contents_id(db_connection = NULL, db_folder = NULL, ...)
}
\arguments{
\item{db_connection}{Defaults to NULL. If NULL, uses local SQLite database.
If given, must be a connection object or a list with relevant connection
settings (see example).}

\item{...}{Passed to \code{cas_get_db_file()}.}
}
\value{
A data frame with three columns and data stored in the \code{contents_id}
table of the local database. The data frame has zero rows if the database
does not exist or no data was previously stored there.
}
\description{
Read contents from local database
}
\examples{
cas_set_options(
  base_folder = fs::path(tempdir(), "R", "castarter_data"),
  db_folder = fs::path(tempdir(), "R", "castarter_data"),
  project = "example_project",
  website = "example_website"
)
cas_enable_db()


urls_df <- cas_build_urls(
  url = "https://www.example.com/news/",
  start_page = 1,
  end_page = 10
)

cas_write_db_contents_id(urls = urls_df$url)

cas_read_db_contents_id()
}
