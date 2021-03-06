% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Main-Functions.R
\name{bin_kurtosis}
\alias{bin_kurtosis}
\title{Bin Kurtosis}
\usage{
bin_kurtosis(trials = 0, prob = 0)
}
\arguments{
\item{trials}{the amount of trials being tested for}

\item{prob}{the probability that a success will occur within a trial}
}
\value{
the value of kurtosis given the above parameters
}
\description{
this function finds the kurtosis given a tests number of trials and probability of success.
}
\examples{
bin_kurtosis(10, 0.3)
}
