% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/var_of_proportion.R
\name{var_of_proportion}
\alias{var_of_proportion}
\title{Variance of a proportion}
\usage{
var_of_proportion(p = NULL, n = NULL)
}
\arguments{
\item{p}{a vector of proportions; each of the proportion values
must be between 0 and 1}

\item{n}{a vector of sample sizes; number of observations used
to calculate each of the percentage values}
}
\description{
Calculate the variance of a proportion.
See Anderson and Finn (1996, p. 364, ISBN: 978-1-4612-8466-6)
}
\examples{
var_of_proportion(p = 0.56, n = 400)
var_of_proportion(p = 0.5, n = 100)
var_of_proportion(p = 0.4, n = 50)
var_of_proportion(p = c(0.5, 0.9), n = c(100, 200))
}
