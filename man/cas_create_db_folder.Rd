% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cas_database.R
\name{cas_create_db_folder}
\alias{cas_create_db_folder}
\title{Creates the base folder where \code{castarter} stores the project database.}
\usage{
cas_create_db_folder(path = NULL, ask = TRUE, ...)
}
\arguments{
\item{ask}{Logical, defaults to TRUE. If FALSE, and database folder does not exist, it just creates it without asking (useful for non-interactive sessions).}
}
\value{
Nothing, used for its side effects.
}
\description{
Creates the base folder where \code{castarter} stores the project database.
}
\examples{
cas_create_db_folder(path = fs::path(fs::path_temp(), "cas_data"), ask = FALSE)
}
\seealso{
Other database functions: 
\code{\link{cas_check_db_folder}()},
\code{\link{cas_check_use_db}()},
\code{\link{cas_connect_to_db}()},
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
