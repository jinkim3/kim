% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mixed_anova_2_way.R
\name{mixed_anova_2_way}
\alias{mixed_anova_2_way}
\title{Mixed ANOVA 2-Way (Two-Way Mixed ANOVA)}
\usage{
mixed_anova_2_way(
  data = NULL,
  iv_name_bw_group = NULL,
  repeated_measures_col_names = NULL,
  iv_name_bw_group_values = NULL,
  colors = NULL,
  error_bar = "ci",
  position_dodge = 0.13,
  legend_title = NULL,
  x_axis_expansion_add = c(0.2, 0.03),
  x_axis_title = NULL,
  y_axis_title = "Mean",
  output = "all"
)
}
\arguments{
\item{data}{a data object (a data frame or a data.table)}

\item{iv_name_bw_group}{name of the between-group independent variable}

\item{repeated_measures_col_names}{names of the columns containing
the repeated measures}

\item{iv_name_bw_group_values}{restrict all analyses to
observations having these values for the between-group
independent variable}

\item{colors}{colors of the dots and lines connecting means
(default = NULL) If there are exactly two repeated measures,
then, by default, \code{colors = c("red", "blue")}}

\item{error_bar}{if \code{error_bar = "ci"} error bars will be a 95\%
confidence interval; if \code{error_bar = "se"}, error bars will be +/-1
standard error. By default, \code{error_bar = "ci"}}

\item{position_dodge}{by how much should the group means and error bars
be horizontally offset from each other so as not to overlap?
(default = 0.13)}

\item{legend_title}{a character for the legend title. If no input
is entered, then, by default, the legend title will be removed.}

\item{x_axis_expansion_add}{inputs for the \code{add} parameter
of the \code{expand} argument. The first and second values respectively
determine the amount of space to add to the left and right along
the x-axis. By default, \code{x_axis_expansion_add = c(0.2, 0.03)} which
means that space with the width of 0.2 will be added to the left, and
space with the width of 0.03 will be added to the right.}

\item{x_axis_title}{a character string for the x-axis title.
If \code{x_axis_title == FALSE}, which is the default value,
the x-axis title will be removed.}

\item{y_axis_title}{a character string for the y-axis title
(default = "Mean"). If \code{x_axis_title == FALSE}, the y-axis title
will be removed.}

\item{output}{output type can be one of the following:
\code{"plot"}, \code{"all"}}
}
\description{
Conduct a two-way mixed analysis of variance (ANOVA).
}
\details{
The following package(s) must be installed prior to running this function:
Package 'afex' v3.0.9 (or possibly a higher version) by
Fox et al. (2020),
\url{https://cran.r-project.org/package=car}
}
\examples{
\donttest{
mixed_anova_2_way(
  data = iris, iv_name_bw_group = "Species",
  repeated_measures_col_names = c("Sepal.Length", "Petal.Length"))
g1 <- mixed_anova_2_way(
  data = iris, iv_name_bw_group = "Species",
  repeated_measures_col_names = c("Sepal.Length", "Petal.Length"),
  error_bar = "se",
  output = "plot")
}
}
