#' Mann-Whitney U Test (Also called Wilcoxon Rank-Sum Test)
#'
#' A nonparametric equivalent of the independent t-test
#'
#' @param data a data object (a data frame or a data.table)
#' @param iv_name name of the independent variable (grouping variable)
#' @param dv_name name of the dependent variable (measure variable
#' of interest)
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
  sigfigs = 3) {
  # bind the vars locally to the function
  iv <- dv <- wilcoxon_rank_sum_p_value <- effect_size_r <- NULL
  # get number of levels in iv
  number_of_lvls_in_iv <- length(unique(data[[iv_name]]))
  if (number_of_lvls_in_iv < 2) {
    stop(paste0(
      "There are fewer than 2 levels in the IV: ",
      sort(unique(data[[iv_name]]))))
  }
  # remove na
  dt01 <- setDT(copy(data))[, c(iv_name, dv_name), with = FALSE]
  names(dt01) <- c("iv", "dv")
  # convert iv to factor
  dt01[, iv := factor(iv)]
  dt01 <- stats::na.omit(dt01)
  # pairs
  group <- sort(unique(dt01$iv))
  dt02 <- data.table(t(utils::combn(group, 2)))
  names(dt02) <- c("group_1", "group_2")
  # group means
  group_1_median <-
    vapply(dt02[["group_1"]], function(i) {
      stats::median(dt01[iv == i]$dv, na.rm = T)},
      FUN.VALUE = numeric(1L))
  group_2_median <-
    vapply(dt02[["group_2"]], function(i) {
      stats::median(dt01[iv == i]$dv, na.rm = T)},
      FUN.VALUE = numeric(1L))
  # wilcoxon test results
  wilcoxon_results <- lapply(1:nrow(dt02), function(i) {
    results_for_1_pair <- stats::wilcox.test(
      formula = dv ~ iv,
      data = dt01[iv %in% dt02[i, ]],
      paired = FALSE)
    # p value and w stat
    wilcoxon_rank_sum_p_value <- results_for_1_pair[["p.value"]]
    w_stat <- results_for_1_pair[["statistic"]]
    # effect size
    z_for_effect_size_r <- stats::qnorm(wilcoxon_rank_sum_p_value / 2)
    n_for_pairwise_comparison <- nrow(dt01[iv %in% dt02[i, ]])
    effect_size_r <- z_for_effect_size_r /
      sqrt(n_for_pairwise_comparison)
    output <- c(wilcoxon_rank_sum_p_value, w_stat, effect_size_r)
    names(output) <- c(
      "wilcoxon_rank_sum_p_value", "w_stat", "effect_size_r")
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
  output[, effect_size_r := signif(effect_size_r, sigfigs)][]
  return(output)
}
