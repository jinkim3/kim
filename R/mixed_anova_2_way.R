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
#' @examples
#' \donttest{
#' mixed_anova_2_way(
#'   data = iris, iv_name_bw_group = "Species",
#'   repeated_measures_col_names = c("Sepal.Length", "Petal.Length"))
#' }
#' @export
#' @import data.table
mixed_anova_2_way <- function(
  data = NULL,
  dv_name = NULL,
  iv_name_bw_group = NULL,
  repeated_measures_col_names = NULL,
  iv_name_bw_group_values = NULL,
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
          "sd" = sd(temp_vector, na.rm = TRUE),
          "se_of_mean" = kim::se_of_mean(temp_vector),
          "ci_95_ll" = ci_of_mean(
            temp_vector, confidence_level = 0.95)[1],
          "ci_95_ul" = ci_of_mean(
            temp_vector, confidence_level = 0.95)[2],
          "median" = median(temp_vector, na.rm = TRUE),
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
  g1 <- g1 + ggplot2::geom_point(size = 5)
  g1 <- g1 + ggplot2::geom_line(linewidth = 1)
  g1 <- g1 + ggplot2::geom_errorbar(
    mapping = ggplot2::aes(
      x = repeated_measure,
      y = mean,
      ymin = ci_95_ll,
      ymax = ci_95_ul),
    width = 0,
    linewidth = 1)
  g1 <- g1 + kim::theme_kim(
    legend_position = "right",
    cap_axis_lines = TRUE)
  g1 <- g1 + ggplot2::scale_x_discrete(
    expand = ggplot2::expansion(add = c(0.2, 0.03)))
  g1 <- g1 + ggplot2::theme(
    axis.title.x = ggplot2::element_blank())
  print(g1)
  if (output == "plot") {
    invisible(g1)
  }
  # return all
  if (output == "all") {
    output <- list(
      "desc_stats_table" = dt3,
      "plot" = g1)
    invisible(output)
  }
}