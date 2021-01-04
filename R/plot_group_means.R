#' Plot group means
#'
#' Creates a plot of sample means and error bars by group.
#'
#' @param data a data object (a data frame or a data.table)
#' @param dv_name name of the dependent variable
#' @param iv_name name(s) of the independent variable(s).
#' Up to two independent variables can be supplied.
#' @param na.rm logical. If \code{na.rm = TRUE}, NA values in
#' independent and dependent variables will be removed before
#' calculating group means.
#' @param error_bar if \code{error_bar = "se"}; error bars will be +/-1
#' standard error, if \code{error_bar = "ci"} error bars will be a
#' confidence interval; if \code{error_bar = "pi"}, error bars will be
#' a prediction interval
#' @param error_bar_range width of the confidence or prediction interval
#' (default = 0.95 for 95 percent confidence or prediction interval).
#' This argument will not apply when \code{error_bar = "se"}
#' @param line_size thickness of the lines connecting group means,
#' (default = 1)
#' @param dot_size size of the dots indicating group means (default = 3)
#' @param error_bar_tip_width graphically, width of the segments
#' at the end of error bars (default = 0.13)
#' @param position_dodge by how much should the group means and error bars
#' be horizontally offset from each other so as not to overlap?
#' (default = 0.13)
#' @param legend_position position of the legend:
#' \code{"none", "top", "right", "bottom", "left", "none"}
#' (default = \code{"right"})
#' @param y_axis_title_vjust position of the y axis title (default = 0.85).
#' If default is used, \code{y_axis_title_vjust = 0.85}, the y axis title
#' will be positioned at 85% of the way up from the bottom of the plot.
#' @return by default, the output will be a ggplot object.
#' If \code{output = "table"}, the output will be a data.table object.
#' @examples
#' plot_group_means(data = mtcars, dv_name = "mpg", iv_name = c("vs", "am"))
#' plot_group_means(
#'   data = mtcars, dv_name = "mpg", iv_name = c("vs", "am"),
#'   error_bar = "se"
#' )
#' plot_group_means(
#'   data = mtcars, dv_name = "mpg", iv_name = c("vs", "am"),
#'   error_bar = "pi", error_bar_range = 0.99
#' )
#' @export
#' @import ggplot2
plot_group_means <- function(
  data = NULL,
  dv_name = NULL,
  iv_name = NULL,
  na.rm = TRUE,
  error_bar = "ci",
  error_bar_range = 0.95,
  line_size = 1,
  dot_size = 3,
  error_bar_tip_width = 0.13,
  position_dodge = 0.13,
  legend_position = "right",
  y_axis_title_vjust = 0.85) {
  # convert to data table
  dt1 <- data.table::setDT(
    data.table::copy(data))[, c(dv_name, iv_name), with = FALSE]
  # remove na
  if (na.rm == TRUE) {
    dt1 <- stats::na.omit(dt1)
  }
  # convert iv to factors
  for (col in iv_name) {
    data.table::set(dt1, j = col, value = as.factor(dt1[[col]]))
  }
  # summary table
  dt2 <- kim::desc_stats_by_group(
    data = dt1,
    var_for_stats = dv_name,
    grouping_vars = iv_name
  )
  # check the number of ivs
  if (length(iv_name) > 2) {
    stop(paste0(
      "The current version of this function cannot handle more ",
      "than two IVs.\nPlease enter one or two IV(s)."
    ))
  }
  # ggplot base
  if (length(iv_name) == 1) {
    g1 <- ggplot(data = dt2, aes(
      y = mean,
      x = get(iv_name),
      group = 1
    )) # connect the dots
  }
  if (length(iv_name) == 2) {
    g1 <- ggplot(data = dt2, aes(
      y = mean,
      x = get(iv_name[1]),
      color = get(iv_name[2]),
      group = get(iv_name[2])
    ))
  }
  # The errorbars will overlap,
  # so use position_dodge to move them horizontally
  pd <- position_dodge(position_dodge)
  # build further
  if (error_bar == "ci") {
    g1 <- g1 + geom_errorbar(aes(
      ymin = dt2$ci_95_ll, ymax = dt2$ci_95_ul
    ),
    width = error_bar_tip_width, size = line_size, position = pd
    )
    error_bar_desc_text <- paste0(
      error_bar_range * 100, "% confidence intervals"
    )
  }
  if (error_bar == "se") {
    g1 <- g1 + geom_errorbar(aes(
      ymin = dt2$mean - dt2$se, ymax = dt2$mean + dt2$se
    ),
    width = error_bar_tip_width, size = line_size, position = pd
    )
    error_bar_desc_text <- "one standard error (+/- 1 SE)"
  }
  if (error_bar == "pi") {
    g1 <- g1 + geom_errorbar(aes(
      ymin = dt2$pi_95_ll, ymax = dt2$pi_95_ul
    ),
    width = error_bar_tip_width, size = line_size, position = pd
    )
    error_bar_desc_text <- paste0(
      error_bar_range * 100, "% prediction intervals"
    )
    if (error_bar_range == 0.95) {
      error_bar_desc_text <- paste0(
        error_bar_desc_text, " (+/- ~1.96 SD)"
      )
    }
  }
  # points and lines
  g1 <- g1 + geom_line(size = line_size, position = pd)
  g1 <- g1 + geom_point(size = dot_size, position = pd)
  if (length(iv_name) == 2) {
    g1 <- g1 + labs(color = iv_name[2])
  }
  g1 <- g1 + xlab(iv_name[1])
  g1 <- g1 + ylab(dv_name)
  g1 <- g1 + labs(
    caption = paste0(
      "\nError bars indicate ", error_bar_desc_text, " around the mean."
    )
  )
  # plot theme
  g1 <- g1 + theme_kim(
    y_axis_title_vjust = y_axis_title_vjust,
    legend_position = legend_position)
  return(g1)
}
