% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_cass_show_ts_dygraph.R
\name{cass_show_ts_dygraph_app}
\alias{cass_show_ts_dygraph_app}
\title{A minimal shiny app that demonstrates the functioning of related modules}
\usage{
cass_show_ts_dygraph_app(count_df)
}
\arguments{
\item{count_df}{A dataframe with three columns (\code{date}, \code{word}, and \code{n}), typically created with \code{cas_count()} and possibly processed with \code{cas_summarise()}.}
}
\value{
A shiny app
}
\description{
A minimal shiny app that demonstrates the functioning of related modules
}
\examples{

count_df <- cas_count(
  corpus = cas_demo_corpus,
  pattern = c("russia", "moscow")
) |> 
  cas_summarise(before = 15, after = 15)

# cass_show_ts_dygraph_app(count_df)
}
