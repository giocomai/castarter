% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/updateDataset.R
\name{UpdateDataset}
\alias{UpdateDataset}
\title{Updates a dataset}
\usage{
UpdateDataset(
  dataset = NULL,
  links = NULL,
  indexLinks = NULL,
  maxNumberOfIndexPages = 1000,
  wait = 1,
  exportRds = TRUE,
  use_headless_chromium = FALSE,
  use_phantomjs = FALSE,
  project = NULL,
  website = NULL
)
}
\arguments{
\item{dataset}{A 'castarter' dataset, defaults to NULL. If not given, and project and website are given (possily, through `SetCastarter(project = ..., website =)`), the latest dataset is automatically loaded.}

\item{links}{A chacter vector, defaults to NULL. If given, it is checked against the given dataset: new links are downloaded, the correspondent pages extracted, and the result added to the dataset. If not given, the function downloads index pages along the criteria found in the stores param file and continues downloading them as long as there is at least a new link in an index page. Based on this, it then proceeds to update the dataset.}

\item{indexLinks}{A chacter vector, defaults to NULL. If given, the presence of new articles is checked after downloading the provided index pages.}

\item{maxNumberOfIndexPages}{An integer, defaults to 1000 to prevent function from running indefinitely (however, this can be set to `Inf`). Maximum number of index pages to be downloaded before the function proceeds with updating the dataset.}

\item{wait}{Defaults to 1. Number of seconds to wait between downloading one page and the next. Can be increased to reduce server load, or can be set to 0 when this is not an issue.}

\item{exportRds}{Logical, defaults to TRUE. If TRUE, it stores the updated dataset in the Dataset folder in .rds format.}

\item{project}{Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. Defaults to NULL, required for importing parameters. This can be left blank if previously set with SetCastarter(project = "project", website = "website").}

\item{website}{Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder. Defaults to NULL, required for importing parameters. This can be left blank if previously set with SetCastarter(project = "project", website = "website").}
}
\value{
A castarter dataset.
}
\description{
Updates a dataset given a previously created parameters export file.
}
\examples{
\dontrun{
dataset <- UpdateDataset()
}
}
