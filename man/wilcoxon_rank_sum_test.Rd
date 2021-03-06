% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wilcoxon_rank_sum_test.R
\name{wilcoxon_rank_sum_test}
\alias{wilcoxon_rank_sum_test}
\title{Wilcoxon Rank-Sum Test (Also called the Mann-Whitney U Test)}
\usage{
wilcoxon_rank_sum_test(
  data = NULL,
  iv_name = NULL,
  dv_name = NULL,
  sigfigs = 3
)
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
Wilcoxon rank-sum test results
}
\description{
A nonparametric equivalent of the independent t-test
}
\examples{
wilcoxon_rank_sum_test(
data = iris, iv_name = "Species", dv_name = "Sepal.Length")
}
