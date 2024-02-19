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
#' @param error_bar_tip_width graphically, width of the segments
#' at the end of error bars (default = 0.13)
#' @param error_bar_thickness thickness of the error bars (default = 1)
#' @param error_bar_caption should a caption be included to indicate
#' the width of the error bars? (default = TRUE).
#' @param lines_connecting_means logical. Should lines connecting means
#' within each group be drawn? (default = TRUE)
#' @param line_colors colors of the lines connecting means (default = NULL)
#' If the second IV has two levels, then by default,
#' \code{line_colors = c("red", "blue")}
#' @param line_types types of the lines connecting means (default = NULL)
#' If the second IV has two levels, then by default,
#' \code{line_types = c("solid", "dashed")}
#' @param line_thickness thickness of the lines connecting group means
#' (default = 1)
#' @param line_size Deprecated. Use the 'linewidth' argument instead.
#' (default = 1)
#' @param dot_size size of the dots indicating group means (default = 3)
#' @param position_dodge by how much should the group means and error bars
#' be horizontally offset from each other so as not to overlap?
#' (default = 0.13)
#' @param x_axis_title a character string for the x-axis title. If no
#' input is entered, then, by default, the first value of
#' \code{iv_name} will be used as the x-axis title.
#' @param y_axis_title a character string for the y-axis title. If no
#' input is entered, then, by default, \code{dv_name} will be used
#' as the y-axis title.
#' @param y_axis_title_vjust position of the y axis title (default = 0.85).
#' By default, \code{y_axis_title_vjust = 0.85}, which means that the
#' y axis title will be positioned at 85% of the way up from the bottom
#' of the plot.
#' @param legend_title a character for the legend title. If no input
#' is entered, then, by default, the second value of \code{iv_name}
#' will be used as the legend title. If \code{legend_title = FALSE},
#' then the legend title will be removed.
#' @param legend_position position of the legend:
#' \code{"none", "top", "right", "bottom", "left", "none"}
#' (default = \code{"right"})
#' @return by default, the output will be a ggplot object.
#' If \code{output = "table"}, the output will be a data.table object.
#' @examples
#' \donttest{
#' plot_group_means(data = mtcars, dv_name = "mpg", iv_name = c("vs", "am"))
#' plot_group_means(
#'   data = mtcars, dv_name = "mpg", iv_name = c("vs", "am"),
#'   error_bar = "se"
#' )
#' plot_group_means(
#'   data = mtcars, dv_name = "mpg", iv_name = c("vs", "am"),
#'   error_bar = "pi", error_bar_range = 0.99
#' )
#' # set line colors and types manually
#' plot_group_means(
#' data = mtcars, dv_name = "mpg", iv_name = c("vs", "am"),
#' line_colors = c("green4", "purple"),
#' line_types = c("solid", "solid"))
#' # remove axis titles
#' plot_group_means(
#' data = mtcars, dv_name = "mpg", iv_name = c("vs", "am"),
#' x_axis_title = FALSE, y_axis_title = FALSE, legend_title = FALSE)
#' }
#' @export
plot_group_means <- function(
    data = NULL,
    dv_name = NULL,
    iv_name = NULL,
    na.rm = TRUE,
    error_bar = "ci",
    error_bar_range = 0.95,
    error_bar_tip_width = 0.13,
    error_bar_thickness = 1,
    error_bar_caption = TRUE,
    lines_connecting_means = TRUE,
    line_colors = NULL,
    line_types = NULL,
    line_thickness = 1,
    line_size = NULL,
    dot_size = 3,
    position_dodge = 0.13,
    x_axis_title = NULL,
    y_axis_title = NULL,
    y_axis_title_vjust = 0.85,
    legend_title = NULL,
    legend_position = "right") {
  # check if Package 'ggplot2' is installed
  if (!"ggplot2" %in% rownames(utils::installed.packages())) {
    message(paste0(
      "This function requires the installation of Package 'ggplot2'.",
      "\nTo install Package 'ggplot2', type ",
      "'kim::prep(ggplot2)'",
      "\n\nAlternatively, to install all packages (dependencies) required ",
      "for all\nfunctions in Package 'kim', type ",
      "'kim::install_all_dependencies()'"))
    return()
  }
  # check arguments
  if (!is.null(line_size)) {
    warning(paste0(
      "The use of the 'line_size' argument was deprecated",
      "\ndue to a change in ggplot2 3.4.0.\n",
      "Going forward, please use the 'line_thickness' argument instead."))
    line_thickness <- line_size
  }
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
    g1 <- ggplot2::ggplot(data = dt2, ggplot2::aes(
      y = mean,
      x = get(iv_name),
      group = 1
    )) # connect the dots
  }
  if (length(iv_name) == 2) {
    g1 <- ggplot2::ggplot(data = dt2, ggplot2::aes(
      y = mean,
      x = get(iv_name[1]),
      color = get(iv_name[2]),
      group = get(iv_name[2]),
      linetype = get(iv_name[2])
    ))
    # number of levels in the second IV
    num_of_levels_in_iv2 <- length(unique(dt2[, get(iv_name[2])]))
  }
  # set defaults if there are two levels in IV 2
  if (num_of_levels_in_iv2 == 2) {
    if (is.null(line_colors)) {
      line_colors = c("red", "blue")
    }
    if (is.null(line_types)) {
      line_types = c("solid", "dashed")
    }
  }
  # set line colors
  if (!is.null(line_colors)) {
    g1 <- g1 + ggplot2::scale_color_manual(
      values = line_colors)
  }
  # set line types
  if (!is.null(line_types)) {
    g1 <- g1 + ggplot2::scale_linetype_manual(
      values = line_types)
  }
  # The errorbars will overlap,
  # so use position_dodge to move them horizontally
  pd <- ggplot2::position_dodge(width = position_dodge)
  # points and lines
  if (lines_connecting_means == TRUE) {
    g1 <- g1 + ggplot2::geom_line(
      linewidth = line_thickness, position = pd)
  }
  g1 <- g1 + ggplot2::geom_point(size = dot_size, position = pd)
  if (length(iv_name) == 2) {
    g1 <- g1 + ggplot2::labs(color = iv_name[2])
  }
  # build further
  if (error_bar == "ci") {
    g1 <- g1 + ggplot2::geom_errorbar(ggplot2::aes(
      ymin = dt2$ci_95_ll, ymax = dt2$ci_95_ul),
      width = error_bar_tip_width,
      linewidth = error_bar_thickness,
      position = pd)
    error_bar_desc_text <- paste0(
      error_bar_range * 100, "% confidence intervals")
  }
  if (error_bar == "se") {
    g1 <- g1 + ggplot2::geom_errorbar(ggplot2::aes(
      ymin = dt2$mean - dt2$se,
      ymax = dt2$mean + dt2$se),
      width = error_bar_tip_width,
      linewidth = error_bar_thickness,
      position = pd)
    error_bar_desc_text <- "one standard error (+/- 1 SE)"
  }
  if (error_bar == "pi") {
    g1 <- g1 + ggplot2::geom_errorbar(ggplot2::aes(
      ymin = dt2$pi_95_ll, ymax = dt2$pi_95_ul),
      width = error_bar_tip_width,
      linewidth = error_bar_thickness,
      position = pd)
    error_bar_desc_text <- paste0(
      error_bar_range * 100, "% prediction intervals"
    )
    if (error_bar_range == 0.95) {
      error_bar_desc_text <- paste0(
        error_bar_desc_text, " (+/- ~1.96 SD)"
      )
    }
  }
  # error bar caption
  if (error_bar_caption == TRUE) {
    g1 <- g1 + ggplot2::labs(
      caption = paste0(
        "\nError bars indicate ", error_bar_desc_text,
        " around the mean."))
  }
  # plot theme
  g1 <- g1 + kim::theme_kim(
    y_axis_title_vjust = y_axis_title_vjust,
    legend_position = legend_position)
  g1 <- g1 + ggplot2::theme(
    legend.spacing.y = ggplot2::unit(1, "cm"),
    legend.key.size = ggplot2::unit(3, "lines"))
  # x axis title
  if (is.null(x_axis_title)) {
    g1 <- g1 + ggplot2::xlab(iv_name[1])
  }  else if (x_axis_title == FALSE) {
    g1 <- g1 + ggplot2::theme(
      axis.title.x = ggplot2::element_blank())
  } else {
    g1 <- g1 + ggplot2::xlab(x_axis_title)
  }
  # y axis title
  if (is.null(y_axis_title)) {
    g1 <- g1 + ggplot2::ylab(dv_name)
  } else if (y_axis_title == FALSE) {
    g1 <- g1 + ggplot2::theme(
      axis.title.y = ggplot2::element_blank())
  } else {
    g1 <- g1 + ggplot2::ylab(y_axis_title)
  }
  # legend
  if (is.null(legend_title)) {
    g1 <- g1 + ggplot2::labs(
      color = iv_name[2],
      linetype = iv_name[2])
  } else if (legend_title == FALSE) {
    g1 <- g1 + ggplot2::guides(
      color = ggplot2::guide_legend(title = NULL),
      linetype = ggplot2::guide_legend(title = NULL)) +
      ggplot2::theme(legend.title = ggplot2::element_blank())
  } else {
    g1 <- g1 + ggplot2::labs(
      color = legend_title,
      linetype = legend_title)
  }
  return(g1)
}
