% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bt_fillmonths.R
\name{bt_fillmonths}
\alias{bt_fillmonths}
\title{Month Fill function updated for Clarifi Data}
\usage{
bt_fillmonths(b, metric, lookback = 12)
}
\arguments{
\item{b}{environment with symbols time series}

\item{metric}{user specified financial metric named according to Clarifi export}
}
\value{

}
\description{
the month fill function will convert quarterly fundamental data
to monthly by filling reported financials forward
}
