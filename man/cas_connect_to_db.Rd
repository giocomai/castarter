% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cas_database.R
\name{cas_connect_to_db}
\alias{cas_connect_to_db}
\title{Return a connection to be used for caching}
\usage{
cas_connect_to_db(
  db_connection = NULL,
  use_db = NULL,
  db_type = NULL,
  db_folder = NULL,
  read_only = FALSE,
  ...
)
}
\arguments{
\item{db_connection}{Defaults to NULL. If NULL, uses local SQLite database.
If given, must be a connection object or a list with relevant connection
settings (see example).}

\item{use_db}{Defaults to NULL. If given, it should be given either TRUE or
FALSE. Typically set with \code{cas_enable_db()} or \code{cas_disable_db()}.}

\item{read_only}{Defaults to FALSE. Passed to \code{DBI::dbConnect}.}

\item{...}{Passed to \code{cas_get_db_file()}.}
}
\value{
A connection object.
}
\description{
Return a connection to be used for caching
}
\examples{
\donttest{
if (interactive()) {
  db_connection <- DBI::dbConnect(
    RSQLite::SQLite(), # or e.g. odbc::odbc(),
    Driver = ":memory:", # or e.g. "MariaDB",
    Host = "localhost",
    database = "example_db",
    UID = "example_user",
    PWD = "example_pwd"
  )
  cas_connect_to_db(db_connection)


  db_settings <- list(
    driver = "MySQL",
    host = "localhost",
    port = 3306,
    database = "castarter",
    user = "secret_username",
    pwd = "secret_password"
  )

  cas_connect_to_db(db_settings)
}
}

}
\seealso{
Other database functions: 
\code{\link{cas_check_db_folder}()},
\code{\link{cas_check_use_db}()},
\code{\link{cas_create_db_folder}()},
\code{\link{cas_disable_db}()},
\code{\link{cas_disconnect_from_db}()},
\code{\link{cas_enable_db}()},
\code{\link{cas_get_db_settings}()},
\code{\link{cas_read_from_db}()},
\code{\link{cas_set_db}()},
\code{\link{cas_set_db_folder}()},
\code{\link{cas_write_to_db}()}
}
\concept{database functions}
