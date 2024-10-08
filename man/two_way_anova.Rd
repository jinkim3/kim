% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/two_way_anova.R
\name{two_way_anova}
\alias{two_way_anova}
\title{Two-Way Factorial ANOVA}
\usage{
two_way_anova(
  data = NULL,
  dv_name = NULL,
  iv_1_name = NULL,
  iv_2_name = NULL,
  iv_1_values = NULL,
  iv_2_values = NULL,
  sigfigs = 3,
  robust = FALSE,
  iterations = 2000,
  plot = TRUE,
  error_bar = "ci",
  error_bar_range = 0.95,
  error_bar_tip_width = 0.13,
  error_bar_thickness = 1,
  error_bar_caption = TRUE,
  line_colors = NULL,
  line_types = NULL,
  line_thickness = 1,
  dot_size = 3,
  position_dodge = 0.13,
  x_axis_title = NULL,
  y_axis_title = NULL,
  y_axis_title_vjust = 0.85,
  legend_title = NULL,
  legend_position = "right",
  output = "anova_table",
  png_name = NULL,
  width = 7000,
  height = 4000,
  units = "px",
  res = 300,
  layout_matrix = NULL
)
}
\arguments{
\item{data}{a data object (a data frame or a data.table)}

\item{dv_name}{name of the dependent variable}

\item{iv_1_name}{name of the first independent variable}

\item{iv_2_name}{name of the second independent variable}

\item{iv_1_values}{restrict all analyses to observations having
these values for the first independent variable}

\item{iv_2_values}{restrict all analyses to observations having
these values for the second independent variable}

\item{sigfigs}{number of significant digits to which to round
values in anova table (default = 3)}

\item{robust}{if \code{TRUE}, conduct a robust ANOVA in addition.}

\item{iterations}{number of bootstrap samples for robust ANOVA.
The default is set at 2000, but consider increasing the number
of samples to 5000, 10000, or an even larger number, if slower
handling time is not an issue.}

\item{plot}{if \code{TRUE}, print a plot and enable returning an output
(default = TRUE)}

\item{error_bar}{if \code{error_bar = "se"}; error bars will be +/-1
standard error; if \code{error_bar = "ci"} error bars will be a
confidence interval}

\item{error_bar_range}{width of the confidence interval
(default = 0.95 for 95 percent confidence interval).
This argument will not apply when \code{error_bar = "se"}}

\item{error_bar_tip_width}{graphically, width of the segments
at the end of error bars (default = 0.13)}

\item{error_bar_thickness}{thickness of the error bars (default = 1)}

\item{error_bar_caption}{should a caption be included to indicate
the width of the error bars? (default = TRUE).}

\item{line_colors}{colors of the lines connecting means (default = NULL)
If the second IV has two levels, then by default,
\code{line_colors = c("red", "blue")}}

\item{line_types}{types of the lines connecting means (default = NULL)
If the second IV has two levels, then by default,
\code{line_types = c("solid", "dashed")}}

\item{line_thickness}{thickness of the lines connecting group means,
(default = 1)}

\item{dot_size}{size of the dots indicating group means (default = 3)}

\item{position_dodge}{by how much should the group means and error bars
be horizontally offset from each other so as not to overlap?
(default = 0.13)}

\item{x_axis_title}{a character string for the x-axis title. If no
input is entered, then, by default, the first value of
\code{iv_name} will be used as the x-axis title.}

\item{y_axis_title}{a character string for the y-axis title. If no
input is entered, then, by default, \code{dv_name} will be used
as the y-axis title.}

\item{y_axis_title_vjust}{position of the y axis title (default = 0.85).
By default, \code{y_axis_title_vjust = 0.85}, which means that the
y axis title will be positioned at 85\% of the way up from the bottom
of the plot.}

\item{legend_title}{a character for the legend title. If no input
is entered, then, by default, the second value of \code{iv_name}
will be used as the legend title. If \code{legend_title = FALSE},
then the legend title will be removed.}

\item{legend_position}{position of the legend:
\code{"none", "top", "right", "bottom", "left", "none"}
(default = \code{"right"})}

\item{output}{output type can be one of the following: \code{"anova_table"},
\code{"group_stats"}, \code{"plot"},
\code{"robust_anova_results"}, \code{"robust_anova_post_hoc_results"},
\code{"robust_anova_post_hoc_contrast"}, \code{"all"}}

\item{png_name}{name of the PNG file to be saved.
If \code{png_name = TRUE}, the name will be "two_way_anova_"
followed by a timestamp of the current time.
The timestamp will be in the format, jan_01_2021_1300_10_000001,
where "jan_01_2021" would indicate January 01, 2021;
1300 would indicate 13:00 (i.e., 1 PM); and 10_000001 would
indicate 10.000001 seconds after the hour.}

\item{width}{width of the PNG file (default = 7000)}

\item{height}{height of the PNG file (default = 4000)}

\item{units}{the units for the \code{width} and \code{height} arguments.
Can be \code{"px"} (pixels), \code{"in"} (inches), \code{"cm"},
or \code{"mm"}. By default, \code{units = "px"}.}

\item{res}{The nominal resolution in ppi which will be recorded
in the png file, if a positive integer. Used for units
other than the default. If not specified, taken as 300 ppi
to set the size of text and line widths.}

\item{layout_matrix}{The layout argument for arranging plots and tables
using the \code{grid.arrange} function.}
}
\value{
by default, the output will be \code{"anova_table"}
}
\description{
This function is deprecated. Use the function 'factorial_anova_2_way'
instead.
}
\details{
Conduct a two-way factorial analysis of variance (ANOVA).

The following package(s) must be installed prior to running this function:
Package 'car' v3.0.9 (or possibly a higher version) by
Fox et al. (2020),
\url{https://cran.r-project.org/package=car}

If robust ANOVA is to be conducted, the following package(s)
must be installed prior to running the function:
Package 'WRS2' v1.1-1 (or possibly a higher version) by
Mair & Wilcox (2021),
\url{https://cran.r-project.org/package=WRS2}
}
\examples{
\dontrun{
two_way_anova(
  data = mtcars, dv_name = "mpg", iv_1_name = "vs",
  iv_2_name = "am", iterations = 100)
anova_results <- two_way_anova(
  data = mtcars, dv_name = "mpg", iv_1_name = "vs",
  iv_2_name = "am", output = "all")
anova_results
}
}
