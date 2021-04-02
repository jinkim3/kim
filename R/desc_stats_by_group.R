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
#' @return the output will be a data.table showing descriptive statistics
#' of the variable for each of the groups formed by the grouping variables.
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
  dt2 <- dt1[!is.na(get(var_for_stats)), list(
    n = length(get(var_for_stats)),
    mean = as.numeric(mean(get(var_for_stats), na.rm = TRUE)),
    sd = as.numeric(stats::sd(get(var_for_stats), na.rm = TRUE)),
    median = as.numeric(stats::median(get(var_for_stats), na.rm = TRUE)),
    min = as.numeric(min(get(var_for_stats), na.rm = TRUE)),
    max = as.numeric(max(get(var_for_stats), na.rm = TRUE)),
    se = as.numeric(se_of_mean(get(var_for_stats), na.rm = TRUE)),
    ci_95_ll = tryCatch(
      as.numeric(stats::t.test(get(var_for_stats))[["conf.int"]][1]),
      warning = function(w) NA_real_, error = function(e) NA_real_),
    ci_95_ul = tryCatch(
      as.numeric(stats::t.test(get(var_for_stats))[["conf.int"]][2]),
      warning = function(w) NA_real_, error = function(e) NA_real_),
    pi_95_ll = tryCatch(
      as.numeric(
        mean(get(var_for_stats), na.rm = TRUE) +
          stats::sd(get(var_for_stats), na.rm = TRUE) *
          stats::qt(0.025, length(get(var_for_stats)) - 1)),
      warning = function(w) NA_real_, error = function(e) NA_real_),
    pi_95_ul = tryCatch(
      as.numeric(
        mean(get(var_for_stats), na.rm = TRUE) +
          stats::sd(get(var_for_stats), na.rm = TRUE) *
          stats::qt(0.975, length(get(var_for_stats)) - 1)),
      warning = function(w) NA_real_, error = function(e) NA_real_),
    skewness = as.numeric(kim::skewness(get(var_for_stats))),
    kurtosis = as.numeric(kim::kurtosis(get(var_for_stats)))),
    keyby = grouping_vars]
  # round to significant digits
  if (!is.null(sigfigs)) {
    if (is.null(cols_to_round)) {
      cols_to_round <- c(
        "mean", "sd", "median", "se", "ci_95_ll", "ci_95_ul", "pi_95_ll",
        "pi_95_ul", "skewness", "kurtosis")
    }
    dt2 <- dt2[, (cols_to_round) := signif(.SD, sigfigs),
               .SDcols = cols_to_round][]
  }
  return(dt2)
}
