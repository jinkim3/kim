#' Mixed ANOVA 2-Way (Two-Way Mixed ANOVA)
#'
#' Conduct a two-way mixed analysis of variance (ANOVA).
#'
#' The following package(s) must be installed prior to running this function:
#' Package 'afex' v3.0.9 (or possibly a higher version) by
#' Fox et al. (2020),
#' <https://cran.r-project.org/package=car>
#'
#' @param data a data object (a data frame or a data.table)
#' @param iv_name_bw_group name of the between-group independent variable
#' @param repeated_measures_col_names names of the columns containing
#' the repeated measures
#' @param iv_name_bw_group_values restrict all analyses to
#' observations having these values for the between-group
#' independent variable
#' @param colors colors of the dots and lines connecting means
#' (default = NULL) If there are exactly two repeated measures,
#' then, by default, \code{line_colors = c("red", "blue")}
#' @param error_bar if \code{error_bar = "ci"} error bars will be a 95%
#' confidence interval; if \code{error_bar = "se"}, error bars will be +/-1
#' standard error. By default, \code{error_bar = "ci"}
#' @param position_dodge by how much should the group means and error bars
#' be horizontally offset from each other so as not to overlap?
#' (default = 0.13)
#' @param legend_title a character for the legend title. If no input
#' is entered, then, by default, the legend title will be removed.
#' @param x_axis_expansion_add inputs for the \code{add} parameter
#' of the \code{expand} argument. The first and second values respectively
#' determine the amount of space to add to the left and right along
#' the x-axis. By default, \code{x_axis_expansion_add = c(0.2, 0.03)} which
#' means that space with the width of 0.2 will be added to the left, and
#' space with the width of 0.03 will be added to the right.
#' @param x_axis_title a character string for the x-axis title.
#' If \code{x_axis_title == FALSE}, which is the default value,
#' the x-axis title will be removed.
#' @param y_axis_title a character string for the y-axis title
#' (default = "Mean"). If \code{x_axis_title == FALSE}, the y-axis title
#' will be removed.
#' @param output output type can be one of the following:
#' \code{"plot"}, \code{"all"}
#' @examples
#' \donttest{
#' mixed_anova_2_way(
#'   data = iris, iv_name_bw_group = "Species",
#'   repeated_measures_col_names = c("Sepal.Length", "Petal.Length"))
#' g1 <- mixed_anova_2_way(
#'   data = iris, iv_name_bw_group = "Species",
#'   repeated_measures_col_names = c("Sepal.Length", "Petal.Length"),
#'   error_bar = "se",
#'   output = "plot")
#' }
#' @export
#' @import data.table
mixed_anova_2_way <- function(
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
  output = "all") {
  # installed packages
  installed_pkgs <- rownames(utils::installed.packages())
  # check if Package 'ggplot2' is installed
  if (!"ggplot2" %in% installed_pkgs) {
    message(paste0(
      "This function requires the installation of Package 'ggplot2'.",
      "\nTo install Package 'ggplot2', type ",
      "'kim::prep(ggplot2)'",
      "\n\nAlternatively, to install all packages (dependencies) required ",
      "for all\nfunctions in Package 'kim', type ",
      "'kim::install_all_dependencies()'"))
    return()
  }
  # bind the vars locally to the function
  p <- NULL
  # convert data to data table
  dt1 <- data.table::setDT(data.table::copy(data))
  dt1 <- dt1[, c(
    iv_name_bw_group, repeated_measures_col_names), with = FALSE]
  # change column names
  data.table::setnames(
    x = dt1,
    old = c(
      iv_name_bw_group, repeated_measures_col_names),
    new = c(
      "iv", paste0("dv_", seq_along(repeated_measures_col_names))))
  # convert the between group iv to a factor
  data.table::set(dt1, j = "iv", value = as.factor(dt1[["iv"]]))
  # remove na
  dt2 <- stats::na.omit(dt1)
  # missing value message
  missing_value_message <- paste0(
    nrow(dt1) - nrow(dt2),
    " rows were removed due to missing values.")
  # count na rows
  message(missing_value_message)
  # subset for certain values
  if (!is.null(iv_name_bw_group_values)) {
    dt2 <- dt2[get("iv") %in% iv_name_bw_group_values]
  }
  # stats by cells
  values_in_bw_group_iv <- kim::su(dt2[["iv"]])
  num_of_dvs <- length(repeated_measures_col_names)
  rows_of_stats <- vector(
    "list", length = length(values_in_bw_group_iv) * num_of_dvs)
  for (i in seq_along(values_in_bw_group_iv)) {
    for (j in seq_len(num_of_dvs)) {
      temp_vector <- dt2[
        iv == values_in_bw_group_iv[i], get(paste0("dv_", j))]
      rows_of_stats[[(i - 1) * num_of_dvs + j]] <-
        data.table::data.table(
          "iv" = values_in_bw_group_iv[i],
          "repeated_measure" = repeated_measures_col_names[j],
          "n" = sum(!is.na(temp_vector)),
          "mean" = mean(temp_vector, na.rm = TRUE),
          "sd" = stats::sd(temp_vector, na.rm = TRUE),
          "se_of_mean" = kim::se_of_mean(temp_vector),
          "ci_95_ll" = kim::ci_of_mean(
            temp_vector, confidence_level = 0.95)[1],
          "ci_95_ul" = kim::ci_of_mean(
            temp_vector, confidence_level = 0.95)[2],
          "median" = stats::median(temp_vector, na.rm = TRUE),
          "min" = min(temp_vector, na.rm = TRUE),
          "max" = min(temp_vector, na.rm = TRUE))
    }
  }
  dt3 <- data.table::rbindlist(rows_of_stats)
  if (output == "desc_stats_table") {
    invisible(dt3)
  }
  # plot
  g1 <- ggplot2::ggplot(
    data = dt3, mapping = ggplot2::aes(
      x = iv,
      y = mean,
      color = repeated_measure,
      group = repeated_measure,
      linetype = repeated_measure))
  # The dots may overlap,
  # so use position_dodge to move them horizontally
  pd <- ggplot2::position_dodge(width = position_dodge)
  g1 <- g1 + ggplot2::geom_point(
    size = 5,
    position = pd)
  g1 <- g1 + ggplot2::geom_line(
    linewidth = 1,
    position = pd)
  # apply colors
  if (num_of_dvs == 2) {
    colors <- c("red", "blue")
  }
  if (!is.null(colors)) {
    g1 <- g1 + ggplot2::scale_color_manual(
      values = colors)
  }
  # error bars
  if (error_bar == "ci") {
    g1 <- g1 + ggplot2::geom_errorbar(
      mapping = ggplot2::aes(
        x = iv,
        y = mean,
        ymin = ci_95_ll,
        ymax = ci_95_ul),
      width = 0,
      linewidth = 1,
      position = pd)
  } else if (error_bar == "se") {
    g1 <- g1 + ggplot2::geom_errorbar(
      mapping = ggplot2::aes(
        x = iv,
        y = mean,
        ymin = mean - se_of_mean,
        ymax = mean + se_of_mean),
      width = 0,
      linewidth = 1,
      position = pd)
  }
  # apply a theme etc
  g1 <- g1 + kim::theme_kim(
    legend_position = "right",
    cap_axis_lines = TRUE)
  g1 <- g1 + ggplot2::scale_x_discrete(
    expand = ggplot2::expansion(add = x_axis_expansion_add))
  # legend
  if (is.null(legend_title)) {
    g1 <- g1 + ggplot2::theme(
      legend.title = ggplot2::element_blank())
  } else {
    g1 <- g1 + ggplot2::labs(
      color = legend_title,
      group = legend_title,
      linetype = legend_title)
  }
  # axis titles
  if (is.null(x_axis_title) || x_axis_title == FALSE) {
    g1 <- g1 + ggplot2::theme(
      axis.title.x = ggplot2::element_blank())
  } else {
    g1 <- g1 + ggplot2::labs(
      x = x_axis_title)
  }
  if (is.null(y_axis_title) || y_axis_title == FALSE) {
    g1 <- g1 + ggplot2::theme(
      axis.title.y = ggplot2::element_blank())
  } else {
    g1 <- g1 + ggplot2::labs(
      x = y_axis_title)
  }
  # print the plot
  print(g1)
  if (output == "plot") {
    invisible(g1)
  } else if (output == "all") {
    # return all
    output <- list(
      "desc_stats_table" = dt3,
      "plot" = g1)
    invisible(output)
  }
}
