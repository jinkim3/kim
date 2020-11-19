#' Descriptive statistics by group
#'
#' Returns descriptive statistics by group
#'
#' @param data a data object (a data frame or a data.table)
#' @param var_for_stats name of the variable for which descriptive
#' statistics will be calculated
#' @param grouping_vars name(s) of grouping variables
#' @return a data.frame
#' @examples
#' desc_stats_by_group(data = mtcars, var_for_stats = "mpg",
#' grouping_vars = c("vs", "am"))
#' @export
desc_stats_by_group <- function(
  data = NULL,
  var_for_stats = NULL,
  grouping_vars = NULL) {
  dt1 <- data.table::setDT(data)
  dt2 <- dt1[, list(
    n = length(get(var_for_stats)),
    mean = mean(get(var_for_stats)),
    sd = stats::sd(get(var_for_stats)),
    median = stats::median(get(var_for_stats)),
    min = min(get(var_for_stats)),
    max = max(get(var_for_stats)),
    se = kim::se_of_mean(get(var_for_stats)),
    ci_95_ll = stats::t.test(get(var_for_stats))[["conf.int"]][1],
    ci_95_ul = stats::t.test(get(var_for_stats))[["conf.int"]][2],
    pi_95_ll = mean(get(var_for_stats)) + stats::sd(get(var_for_stats)) *
      stats::qt(0.025, length(get(var_for_stats)) - 1),
    pi_95_ul = mean(get(var_for_stats)) + stats::sd(get(var_for_stats)) *
      stats::qt(0.975, length(get(var_for_stats)) - 1),
    skewness = moments::skewness(get(var_for_stats)),
    kurtosis = moments::kurtosis(get(var_for_stats))),
    keyby = grouping_vars]
  return(dt2)
}
