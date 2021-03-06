% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preliminary.R
\name{CreateFolders}
\alias{CreateFolders}
\title{Creates the folder structure needed by 'castarter'.}
\usage{
CreateFolders(project = NULL, website = NULL)
}
\arguments{
\item{project}{Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.}

\item{website}{Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.}
}
\description{
Creates all the standard folders required by a 'castarter' project.
}
\examples{
\dontrun{
CreateFolders(project, website)
}
}
