% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mann_whitney.R
\name{mann_whitney}
\alias{mann_whitney}
\title{Mann-Whitney U Test (Also called Wilcoxon Rank-Sum Test)}
\usage{
mann_whitney(data = NULL, iv_name = NULL, dv_name = NULL, sigfigs = 3)
}
\arguments{
\item{data}{a data object (a data frame or a data.table)}

\item{iv_name}{name of the independent variable (grouping variable)}

\item{dv_name}{name of the dependent variable (measure variable
of interest)}

\item{sigfigs}{number of significant digits to round to}
}
\value{
the output will be a data.table object with all pairwise
Mann-Whitney test results
}
\description{
A nonparametric equivalent of the independent t-test
}
\examples{
mann_whitney(data = iris, iv_name = "Species", dv_name = "Sepal.Length")
}
