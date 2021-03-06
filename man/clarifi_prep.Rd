% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clarifi_prep.R
\name{clarifi_prep}
\alias{clarifi_prep}
\title{Prep Exported Data from Clarifi}
\usage{
clarifi_prep(clarifi_data, type = c("fundamental", "price"))
}
\arguments{
\item{clarifi_data}{Clarifi exported data read into a data.frame. Should have date,sector,ticker, and quarterly indicator in first four columns in that order, followed by any order of fundamental metrics. For pricing data, should have date, ticker, high, low, close, volume, dividends per share, and market capitalization.}

\item{type}{type of export data either "fundamental" or "price"}
}
\value{
environment variable for fundamental or pricing data.
}
\description{
Prep Exported Data from Clarifi
}
