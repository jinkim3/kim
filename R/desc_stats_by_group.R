#' Descriptive statistics by group
#'
#' Returns descriptive statistics by group
#'
#' @param data a data object (a data frame or a data.table)
#' @param var_for_stats name of the variable for which descriptive
#' statistics will be calculated
#' @param grouping_vars name(s) of grouping variables
#' @param sigfigs number of significant digits to round to
#' @param cols_to_round names of columns whose values will be rounded
#' @return a data.frame
#' @examples
#' desc_stats_by_group(data = mtcars, var_for_stats = "mpg",
#' grouping_vars = c("vs", "am"))
#' @export
#' @import data.table
desc_stats_by_group <- function(
  data = NULL,
  var_for_stats = NULL,
  grouping_vars = NULL,
  sigfigs = NULL,
  cols_to_round = NULL) {
  dt1 <- setDT(copy(data))
  dt2 <- dt1[, list(
    n = as.numeric(length(get(var_for_stats))),
    mean = as.numeric(mean(get(var_for_stats))),
    sd = as.numeric(stats::sd(get(var_for_stats))),
    median = as.numeric(stats::median(get(var_for_stats))),
    min = as.numeric(min(get(var_for_stats))),
    max = as.numeric(max(get(var_for_stats))),
    se = as.numeric(kim::se_of_mean(get(var_for_stats))),
    ci_95_ll = as.numeric(
      stats::t.test(get(var_for_stats))[["conf.int"]][1]),
    ci_95_ul = as.numeric(
      stats::t.test(get(var_for_stats))[["conf.int"]][2]),
    pi_95_ll = as.numeric(
      mean(get(var_for_stats)) + stats::sd(get(var_for_stats)) *
      stats::qt(0.025, length(get(var_for_stats)) - 1)),
    pi_95_ul = as.numeric(
      mean(get(var_for_stats)) + stats::sd(get(var_for_stats)) *
      stats::qt(0.975, length(get(var_for_stats)) - 1)),
    skewness = as.numeric(
      moments::skewness(get(var_for_stats))),
    kurtosis = as.numeric(
      moments::kurtosis(get(var_for_stats)))),
    keyby = grouping_vars]
  # round to significant digits
  if (!is.null(sigfigs)) {
    if (is.null(cols_to_round)) {
      cols_to_round <- c(
        "mean", "sd", "median", "se", "ci_95_ll", "ci_95_ul", "pi_95_ll",
        "pi_95_ul", "skewness", "kurtosis")
      dt2 <- dt2[, (cols_to_round) := signif(.SD, sigfigs),
          .SDcols = cols_to_round][]
    }
  }
  return(dt2)
}
