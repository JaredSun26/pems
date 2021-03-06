% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter.missing.R
\name{filter.missing}
\alias{filter.missing}
\title{A function to apply missing data filter on a dataset}
\usage{
filter.missing(tables, hole.size = 6)
}
\arguments{
\item{tables}{a list in which each element is a data dataframe for one day's traffic data. It could be the output of read.processed().}

\item{hole.size}{The threshold for the longest NA sequence}
}
\description{
A function to apply missing data filter on a dataset
}
\examples{
tables<-filter.missing(tables, hole.size=6)
}

