% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cas_database.R
\name{cas_get_db_file}
\alias{cas_get_db_file}
\title{Gets location of database file}
\usage{
cas_get_db_file(db_folder = NULL, ...)
}
\value{
A character vector of length one with location of the SQLite database file.
}
\description{
Gets location of database file
}
\examples{

cas_set_db_folder(path = tempdir())
db_file_location <- cas_get_db_file(project = "test-project") # outputs location of database file
db_file_location
}
