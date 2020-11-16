#' Plot group means
#'
#' @param data a data object (a data frame or a data.table)
#' @param dv_name a name (character string) of the dependent variable
#' @param iv_name name(s) of the independent variable(s).
#' Up to two independent variables can be supplied.
#' @param ci if \code{TRUE}, error bars will be a confidence interval
#' @param se if \code{TRUE}, error bars will be +/-1 standard error
#' @param pi if \code{TRUE}, error bars will be a prediction interval
#' @param ci_range width of the confidence interval
#' (default = 0.95 for 95 percent confidence interval)
#' @param pi_range width of the prediction interval
#' (default = 0.95 for 95 percent prediction interval)
#' @param line_size thickness of the lines connecting group means,
#' (default = 1)
#' @param dot_size size of the dots indicating group means (default = 3)
#' @param error_bar_width graphically, width of the segments
#' at the end of error bars (default = 0.13)
#' @param position_dodge by how much should the group means and error bars
#' be horizontally offset from each other so as not to overlap?
#' (default = 0.13)
#' @param legend_position position of the legend:
#' "none", "top", "right", "bottom", "left", "none" (default = "right")
#' @param output output type: either "plot" or "table"
#' @return by default, the output will be a ggplot object.
#' If \code{output = "table"}, the output will be a data.table object.
#' @examples
#' plot_group_means(data = mtcars, dv_name = "mpg", iv_name = "gear")
#' @export
#' @import ggplot2
plot_group_means <- function(
  data = NULL,
  dv_name = NULL,
  iv_name = NULL,
  ci = NULL,
  se = NULL,
  pi = NULL,
  ci_range = 0.95,
  pi_range = 0.95,
  line_size = 1,
  dot_size = 3,
  error_bar_width = 0.13,
  position_dodge = 0.13,
  legend_position = "right",
  output = "plot") {
  # summary table
  dt1 <- Rmisc::summarySE(
    data = data,
    measurevar = dv_name,
    groupvars = iv_name,
    conf.interval = ci_range)
  # add 95% prediction intervals
  dt1[["pi"]] <- dt1$sd * stats::qt(pi_range/2 + 0.5, dt1$N - 1)
  # output to a table
  if(output == "table") {
    requireNamespace("data.table")
    data.table::setDT(dt1)
    return(dt1)
  }
  # ci vs se vs pi
  num_of_error_bar_args <- sum(
    c(!is.null(ci), !is.null(se), !is.null(pi)))
  if(num_of_error_bar_args == 0) {
    ci <- F
    se <- F
    pi <- T
  } else if(num_of_error_bar_args >= 2) {
    # conflicting arguments
    stop(paste0(
      "Please set only ONE of the following arguments to be TRUE: ",
      "ci, se, or pi"))
  }
  # check the number of ivs
  if(length(iv_name) > 2) {
    stop(paste0(
      "The current version of this function cannot handle more ",
      "than two IVs.\nPlease enter one or two IV(s)."))
  }
  # ggplot base
  if(length(iv_name) == 1) {
    g1 <- ggplot(data = dt1, aes(
      y = get(dv_name),
      x = get(iv_name),
      group = 1)) # connect the dots
  }
  if(length(iv_name) == 2) {
    g1 <- ggplot(data = dt1, aes(
      y = get(dv_name),
      x = get(iv_name[1]),
      color = get(iv_name[2]),
      group = get(iv_name[2])))
  }
  # The errorbars will overlap,
  # so use position_dodge to move them horizontally
  pd <- position_dodge(position_dodge)
  # build further
  if(ci) {
    g1 <- g1 + geom_errorbar(aes(
      ymin = get(dv_name) - ci, ymax = get(dv_name) + ci),
      width = error_bar_width, size = line_size, position = pd)
    error_bar_desc_text <- paste0(
      ci_range * 100, "% confidence intervals")
  }
  if(se) {
    g1 <- g1 + geom_errorbar(aes(
      ymin = get(dv_name) - se, ymax = get(dv_name) + se),
      width = error_bar_width, size = line_size, position = pd)
    error_bar_desc_text <- "one standard error (+/- 1 SE)"
  }
  if(pi) {
    g1 <- g1 + geom_errorbar(aes(
      ymin = get(dv_name) - pi, ymax = get(dv_name) + pi),
      width = error_bar_width, size = line_size, position = pd)
    error_bar_desc_text <- paste0(
      pi_range * 100, "% prediction intervals")
    if(pi_range == 0.95) {
      error_bar_desc_text <- paste0(
        error_bar_desc_text, " (+/- ~1.96 SD)")
    }
  }
  # points and lines
  g1 <- g1 + geom_line(size = line_size, position = pd)
  g1 <- g1 + geom_point(size = dot_size, position = pd)
  if(length(iv_name) == 2) {
    g1 <- g1 + theme(legend.position = legend_position)
    g1 <- g1 + labs(color = iv_name[2])
  }
  g1 <- g1 + xlab(iv_name[1])
  g1 <- g1 + ylab(dv_name)
  g1 <- g1 + labs(
    caption = paste0(
      "\nError bars indicate ", error_bar_desc_text, " around the mean."))
  return(g1)
}
