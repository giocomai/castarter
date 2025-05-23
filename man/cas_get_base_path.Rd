% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cas_get_base_path.R
\name{cas_get_base_path}
\alias{cas_get_base_path}
\title{Build full path to base folder where batches of files will be stored.}
\usage{
cas_get_base_path(
  create_folder_if_missing = NULL,
  custom_path = NULL,
  custom_folder = NULL,
  index = FALSE,
  file_format = "html",
  ...
)
}
\arguments{
\item{create_folder_if_missing}{Logical, defaults to NULL. If NULL, it will
ask before creating a new folder. If TRUE, it will create it without
asking.}

\item{custom_path}{Defaults to NULL. If given, overrides all other inputs,
and is returned as given.}

\item{custom_folder}{Defaults to NULL. Folder name within the website folder
is typically determined by \code{file_format} and either \code{index} or \code{contents};
if you prefer another option, different from both \code{index} and \code{contents},
you can set it using the \code{custom_folder} argument.}

\item{file_format}{Defaults to \code{html}. Used to determine folder name, and
internally to assume contents type when using functions such as
\code{\link[=cas_extract]{cas_extract()}}.}

\item{...}{Passed to \code{\link[=cas_get_options]{cas_get_options()}}.}
}
\value{
Path to base folder. A character vector of length one of class
\code{fs_path}.
}
\description{
For more information on how the full path is determined, see details.
}
\details{
\itemize{
\item \code{base_folder} - path to the folder where all project files are expected to be stored. Can be retrieved with \code{cas_get_base_folder(level = "base")}, can be set for the whole session with \verb{cas_set_options(base_folder = fs::path(fs::path_temp(), "castarter")}.
\item \code{project} - project name, typically set with \verb{cas_set_options(project = "example_project)}
\item \code{website} - website name, typically set with \verb{cas_set_options(website = "example_project)}
\item a combination of \code{file_format} and either \code{index}, \code{contents}, or something different if set with the \code{custom_folder} argument.
}
}
\examples{
set.seed(1)
cas_set_options(project = "example_project",
                website = "example_website", 
                base_folder = fs::path(fs::path_temp(), "castarter"))

cas_get_base_path(create_folder_if_missing = FALSE)

cas_get_base_path(file_format = "html", 
                  create_folder_if_missing = FALSE)

cas_get_base_path(file_format = "xml", 
                  create_folder_if_missing = FALSE)

cas_get_base_path(index = TRUE,
                  create_folder_if_missing = FALSE)
}
