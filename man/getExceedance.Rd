% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/metocean.R
\name{getExceedance}
\alias{getExceedance}
\title{Function to calculate exceedance plots and tables}
\usage{
getExceedance(df, variable, percentiles = c(c(0, 0.01, 0.02, 0.05), seq(0.1,
  0.9, 0.1), c(0.95, 0.98, 0.99, 1)), xlabel, type = "annual",
  hemisphere = "northern", output_type = "plot")
}
\arguments{
\item{df}{time series of metocean data}

\item{variable}{index of variable for plotting}

\item{percentiles}{list of percentiles between 0 and 1}

\item{xlabel}{string input of xlab for plots}

\item{type}{string 'monthly' 'season'}

\item{hemisphere}{southern or northern}
}
\value{
dataframe of exceedances
}
\description{
Function to calculate exceedance plots and tables
}
\examples{
getExceedance()
}

