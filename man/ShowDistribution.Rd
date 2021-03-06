% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timeSeries.R
\name{ShowDistribution}
\alias{ShowDistribution}
\title{Creates a time series graph showing the distribution of documents by date}
\usage{
ShowDistribution(
  dataset,
  specificWebsites = NULL,
  rollingDays = 31,
  align = "center",
  customTitle = NULL,
  method = "numberOfArticles",
  export = FALSE,
  project = NULL,
  website = NULL
)
}
\arguments{
\item{dataset}{A dataset created with 'castarter'.}

\item{specificWebsites}{Character vector indicating which websites (defined by relative website) have to be included in graph. If left to default, includes all websites present in the dataset.}

\item{rollingDays}{Integer, defaults to 31. Number of days used to calculate word frequency as shown in the time series. Time series shows word frequency for each date as an average of the N number of days (N=rollingDays) following the correspondent date.}

\item{align}{Defaults to "center", can be either "left", "right" or "center" and refers to the way the rolling average is calculated.}

\item{method}{Accepted values: "numberOfArticles" (default, creates time series based on number of publications per day); "numberOfCharacters" (creates time series based on number of charachters per day, currently does not work in conjunction with specificWebsites option).}

\item{export}{Logical, defaults to FALSE. If TRUE, saves the graph in both png and pdf format. If project and website are provided, in saves the timeseries in the correspondent "Outputs" subfolder.}

\item{project}{Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.}

\item{website}{Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.}

\item{startDate, }{endDate Character vector with date in the format year-month-date, e.g. "2015-07-14".}
}
\value{
A ggplot2 time series showing number of articles published each day.
}
\description{
Creates a time series graph showing the distribution of documents by date.
}
\examples{
\dontrun{
ShowDistribution(dataset)
}
}
