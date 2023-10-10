#' Mann-Whitney U Test (Also called Wilcoxon Rank-Sum Test)
#'
#' A nonparametric equivalent of the independent t-test
#'
#' @param data a data object (a data frame or a data.table)
#' @param iv_name name of the independent variable (grouping variable)
#' @param dv_name name of the dependent variable (measure variable
#' of interest)
#' @param iv_level_order order of levels in the independent
#' variable. By default, it will be set as levels of the
#' independent variable ordered using R's base function \code{sort}.
#' @param sigfigs number of significant digits to round to
#' @return the output will be a data.table object with all pairwise
#' Mann-Whitney test results
#' @examples
#' mann_whitney(data = iris, iv_name = "Species", dv_name = "Sepal.Length")
#' @export
#' @import data.table
mann_whitney <- function(
  data = NULL,
  iv_name = NULL,
  dv_name = NULL,
  iv_level_order = NULL,
  sigfigs = 3) {
  # bind the vars locally to the function
  iv <- dv <- wilcoxon_rank_sum_p_value <- effect_size_r_abs_value <- NULL
  # get number of levels in iv
  number_of_lvls_in_iv <- length(unique(data[[iv_name]]))
  if (number_of_lvls_in_iv < 2) {
    stop(paste0(
      "There are fewer than 2 levels in the IV: ",
      paste0(sort(unique(data[[iv_name]])), collapse = ", ")))
  }
  # create a new dt
  dt01 <- data.table::setDT(
    data.table::copy(data))[, c(iv_name, dv_name), with = FALSE]
  names(dt01) <- c("iv", "dv")
  # remove na
  dt01 <- stats::na.omit(dt01)
  # set the order of levels in iv
  if (is.null(iv_level_order)) {
    group <- sort(unique(dt01$iv))
  } else {
    # if iv is a factor
    if (is.factor(dt01$iv)) {
      if (setequal(kim::su(levels(dt01$iv)), iv_level_order) == FALSE) {
        stop(paste0(
          "The levels of independent variables do not match:\n",
          "Input for `iv_level_order`: ",
          paste0(iv_level_order, collapse = ", "),
          "\nLevels of IV in the data set: ",
          paste0(kim::su(levels(dt01$iv)), collapse = ", ")))
      }
      dt01[, iv := factor(iv, levels = iv_level_order)]
      group <- iv_level_order
    } else {
      # if iv is not a factor
      if (setequal(kim::su(dt01$iv), iv_level_order) == FALSE) {
        stop(paste0(
          "The levels of independent variables do not match:\n",
          "Input for `iv_level_order`: ",
          paste0(iv_level_order, collapse = ", "),
          "\nLevels of IV in the data set: ",
          paste0(kim::su(levels(dt01$iv)), collapse = ", ")))
      }
      group <- iv_level_order
    }
  }
  # possible pairwise combinations of iv levels
  dt02 <- data.table(t(utils::combn(group, 2)))
  names(dt02) <- c("group_1", "group_2")
  # group means
  group_1_median <-
    vapply(dt02[["group_1"]], function(i) {
      stats::median(dt01[iv == i]$dv, na.rm = TRUE)},
      FUN.VALUE = numeric(1L))
  group_2_median <-
    vapply(dt02[["group_2"]], function(i) {
      stats::median(dt01[iv == i]$dv, na.rm = TRUE)},
      FUN.VALUE = numeric(1L))
  # wilcoxon test results
  wilcoxon_results <- lapply(seq_len(nrow(dt02)), function(i) {
    results_for_1_pair <- stats::wilcox.test(
      formula = dv ~ iv,
      data = dt01[iv %in% dt02[i, ]])
    # p value and w stat
    wilcoxon_rank_sum_p_value <- results_for_1_pair[["p.value"]]
    w_stat <- results_for_1_pair[["statistic"]]
    # effect size
    z_for_effect_size_r <- stats::qnorm(wilcoxon_rank_sum_p_value / 2)
    n_for_pairwise_comparison <- nrow(dt01[iv %in% dt02[i, ]])
    effect_size_r_abs_value <- abs(
      z_for_effect_size_r /
        sqrt(n_for_pairwise_comparison))
    output <- c(
      wilcoxon_rank_sum_p_value, w_stat, effect_size_r_abs_value)
    names(output) <- c(
      "wilcoxon_rank_sum_p_value", "w_stat", "effect_size_r_abs_value")
    return(output)})
  wilcoxon_results_dt <- as.data.table(
    do.call(rbind, wilcoxon_results))
  # bonferroni sig
  bonferroni_signif_for_wilcoxon_test <- ifelse(
    wilcoxon_results_dt[, wilcoxon_rank_sum_p_value] < (.05 / nrow(dt02)),
    "Yes", "No")
  # put all the results together as a data table
  output <- data.table(
    dt02,
    group_1_median,
    group_2_median,
    wilcoxon_results_dt)
  # round values
  output[, wilcoxon_rank_sum_p_value :=
           kim::pretty_round_p_value(wilcoxon_rank_sum_p_value)][]
  output[, effect_size_r_abs_value := kim::round_flexibly(
    effect_size_r_abs_value, sigfigs)][]
  return(output)
}
