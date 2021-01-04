#' Two-way ANOVA
#'
#' @param data a data object (a data frame or a data.table)
#' @param dv_name name of the dependent variable
#' @param iv_1_name name of the first independent variable
#' @param iv_2_name name of the second independent variable
#' @param iv_1_values restrict all analyses to observations having
#' these values for the first independent variable
#' @param iv_2_values restrict all analyses to observations having
#' these values for the second independent variable
#' @param robust if \code{TRUE}, conduct a robust ANOAVA in addition.
#' @param iterations number of bootstrap samples for robust ANOVA.
#' The default is set at 2000, but consider increasing the number
#' of samples to 5000, 10000, or an even larger number, if slower
#' handling time is not an issue.
#' @param plot if \code{TRUE}, print a plot and enable returning an output.
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
#' @param output output type can be one of the following: \code{"anova_table"},
#' \code{"group_stats"}, \code{"plot"}, \code{"levene_test_result"},
#' \code{"robust_anova_results"}, \code{"robust_anova_post_hoc_results"},
#' \code{"robust_anova_post_hoc_contrast"}
#' @return by default, the output will be \code{"anova_table"}
#' @examples
#' two_way_anova(
#'   data = mtcars, dv_name = "mpg", iv_1_name = "vs",
#'   iv_2_name = "am", iterations = 100
#' )
#' @export
#' @import ggplot2 data.table
#' @importFrom car Anova leveneTest
two_way_anova <- function(
  data = NULL,
  dv_name = NULL,
  iv_1_name = NULL,
  iv_2_name = NULL,
  iv_1_values = NULL,
  iv_2_values = NULL,
  robust = FALSE,
  iterations = 2000,
  plot = FALSE,
  error_bar = "ci",
  error_bar_range = 0.95,
  line_size = 1,
  dot_size = 3,
  error_bar_tip_width = 0.13,
  position_dodge = 0.13,
  legend_position = "right",
  output = NULL) {
  # convert data to data table
  dt1 <- data.table::setDT(copy(data))
  dt1 <- dt1[, c(iv_1_name, iv_2_name, dv_name), with = FALSE]
  # convert iv to factors
  for (col in c(iv_1_name, iv_2_name)) {
    data.table::set(dt1, j = col, value = as.factor(dt1[[col]]))
  }
  # remove na
  dt2 <- stats::na.omit(dt1)
  # count na rows
  message(paste0(
    nrow(dt1) - nrow(dt2),
    " rows were removed due to missing values."
  ))
  # subset for certain values
  if (!is.null(iv_1_values)) {
    dt2 <- dt2[get(iv_1_name) %in% iv_1_values]
  }
  if (!is.null(iv_2_values)) {
    dt2 <- dt2[get(iv_2_name) %in% iv_2_values]
  }
  # stats by iv
  group_stats <- dt2[, list(
    n = .N,
    mean = mean(get(dv_name)),
    median = as.numeric(stats::median(get(dv_name))),
    sd = stats::sd(get(dv_name)),
    se = kim::se_of_mean(get(dv_name)),
    min = min(get(dv_name)),
    max = max(get(dv_name))
  ),
  keyby = c(iv_1_name, iv_2_name)
  ]
  if (output == "group_stats") {
    return(group_stats)
  }
  message(paste0("\nGroup Statistics on ", dv_name, ":"))
  print(group_stats)
  # print or return plot
  if (plot == TRUE | output == "plot") {
    g1 <- kim::plot_group_means(
      data = dt2,
      dv_name = dv_name,
      iv_name = c(iv_1_name, iv_2_name),
      error_bar = error_bar,
      error_bar_range = error_bar_range,
      line_size = line_size,
      dot_size = dot_size,
      error_bar_tip_width = error_bar_tip_width,
      position_dodge = position_dodge,
      legend_position = legend_position)
    if (output == "plot") {
      return(g1)
    }
    print(g1)
  }
  # order the data table
  data.table::setorderv(dt2, c(iv_1_name, iv_2_name))
  # levene's test
  formula_1 <- stats::as.formula(
    paste0(dv_name, " ~ ", iv_1_name, " * ", iv_2_name)
  )
  levene_test_result <- car::leveneTest(
    y = formula_1, data = dt2
  )
  levene_test_p_value <- levene_test_result[["Pr(>F)"]][1]
  message("\nLevene's Test Results:")
  print(levene_test_result)
  if (levene_test_p_value < .05) {
    message(paste0(
      "The homogeneity of variance assumption is violated."))
  }
  if (output == "levene_test_result") {
    return(levene_test_result)
  }
  # anova instead of regression
  model_1 <- stats::aov(formula = formula_1, data = dt2)
  anova_table <- car::Anova(model_1, type = 3)
  message("\nANOVA Results:")
  print(anova_table)
  # output by type
  if (output == "anova_table") {
    return(anova_table)
  }
  if (robust == TRUE) {
    # robust anova
    robust_anova_results <-
      WRS2::pbad2way(formula_1,
                     data = dt2, est = "mom",
                     nboot = iterations
      )
    if (output == "robust_anova_results") {
      return(robust_anova_results)
    }
    message("\nRobust ANOVA Results:")
    print(robust_anova_results)
    robust_anova_post_hoc_results <-
      WRS2::mcp2a(formula_1,
                  data = dt2, est = "mom",
                  nboot = iterations
      )
    message("\nRobust ANOVA Post Hoc Test Results:")
    print(robust_anova_post_hoc_results)
    # contrasts
    robust_anova_post_hoc_contrast <-
      robust_anova_post_hoc_results[["contrasts"]]
    message("\nRobust ANOVA Post Hoc Test Contrasts:")
    print(robust_anova_post_hoc_contrast)
  }
  # default output
  if (is.null(output)) {
    output <- "anova_table"
  }
  if (output == "robust_anova_post_hoc_results") {
    invisible(robust_anova_post_hoc_results)
  }
  if (output == "robust_anova_post_hoc_contrast") {
    invisible(robust_anova_post_hoc_contrast)
  }
}
