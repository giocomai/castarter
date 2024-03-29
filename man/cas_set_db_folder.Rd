% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cas_database.R
\name{cas_set_db_folder}
\alias{cas_set_db_folder}
\alias{cas_get_db_folder}
\title{Set folder for storing the database}
\usage{
cas_set_db_folder(path = NULL, ...)

cas_get_db_folder(path = NULL, ...)
}
\arguments{
\item{path}{A path to a location used for storing the database. If the folder does not exist, it will be created.}
}
\value{
The path to the database folder, if previously set; the same path as given to the function; or the default, \code{cas_data} is none is given.
}
\description{
Consider using a folder out of your current project directory, e.g. \code{cas_set_db_folder("~/R/cas_data/")}: you will be able to use the same database in different projects, and prevent database files from being sync-ed if you use services such as Nextcloud or Dropbox.
}
\examples{
cas_set_db_folder(fs::path(fs::path_home_r(), "R", "cas_data"))

cas_set_db_folder(fs::path(fs::path_temp(), "cas_data"))
cas_get_db_folder()
}
\seealso{
Other database functions: 
\code{\link{cas_check_db_folder}()},
\code{\link{cas_check_use_db}()},
\code{\link{cas_connect_to_db}()},
\code{\link{cas_create_db_folder}()},
\code{\link{cas_disable_db}()},
\code{\link{cas_disconnect_from_db}()},
\code{\link{cas_enable_db}()},
\code{\link{cas_get_db_settings}()},
\code{\link{cas_read_from_db}()},
\code{\link{cas_set_db}()},
\code{\link{cas_write_to_db}()}
}
\concept{database functions}
