% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_raw_data.R
\name{get_current_communes}
\alias{get_current_communes}
\title{get_current_communes Downloads list of current communes from Wikipedia}
\usage{
get_current_communes(
  url = "https://en.wikipedia.org/wiki/List_of_communes_of_Luxembourg",
  table_position = 1
)
}
\arguments{
\item{url}{Optional: Persistent url to the data}

\item{table_position}{Optional: Scraping returns a list of tables, so users need to specify the correct table. Defaults to 1, the position of the table as of writing.}
}
\value{
A data frame
}
\description{
get_current_communes Downloads list of current communes from Wikipedia
}
