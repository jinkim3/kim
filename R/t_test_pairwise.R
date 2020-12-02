#' t test, pairwise
#'
#' Conducts a t-test for every possible pairwise comparison
#' with Bonferroni correction
#'
#' @param data a data object (a data frame or a data.table)
#' @param iv_name name of the independent variable
#' @param dv_name name of the dependent variable
#' @param sigfigs number of significant digits to round to
#' @return the output will be a data.table
#' @examples
#' t_test_pairwise(data = iris, iv_name = "Species", dv_name = "Sepal.Length")
#' @export
#' @import data.table
t_test_pairwise <- function(
  data = NULL,
  iv_name = NULL,
  dv_name = NULL,
  sigfigs = 3) {
  # bind the vars locally to the function
  iv <- dv <- NULL
  # check number of iv_name and dv_name
  if (length(iv_name) > 1) {
    message(paste0(
      "The current version can handle only one independent variable.\n",
      "Please enter only one IV."))
  }
  # remove na
  dt01 <- setDT(copy(data))[, c(iv_name, dv_name), with = F]
  names(dt01) <- c("iv", "dv")
  # convert iv to factor
  dt01[, iv := factor(iv)]
  dt01 <- stats::na.omit(dt01)
  # pairs
  group <- sort(unique(dt01$iv))
  dt02 <- data.table(t(utils::combn(group, 2)))
  names(dt02) <- c("group_1", "group_2")
  # group means
  group_1_mean <-
    vapply(dt02$group_1, function(i) {mean(dt01[iv == i]$dv, na.rm = T)},
           FUN.VALUE = numeric(1))
  group_2_mean <-
    vapply(dt02$group_2, function(i) {mean(dt01[iv == i]$dv, na.rm = T)},
           FUN.VALUE = numeric(1))
  # cohen d
  cohen_d <- vapply(1:nrow(dt02), function(i) {
    temp <- dt01[iv %in% dt02[i, ]]
    temp[, iv := factor(iv)]
    effsize::cohen.d(dv ~ iv, temp)[["estimate"]]},
    FUN.VALUE = numeric(1))
  # t test p values
  t_test_p_value <- vapply(1:nrow(dt02), function(i) {
    stats::t.test(dv ~ iv, dt01[iv %in% dt02[i, ]])[["p.value"]]},
    FUN.VALUE = numeric(1))
  # mann whitney test p values
  mann_whitney_p_value <- vapply(1:nrow(dt02), function(i) {
    stats::wilcox.test(dv ~ iv, dt01[iv %in% dt02[i, ]])[["p.value"]]},
    FUN.VALUE = numeric(1))
  # bonferroni sig
  bonferroni_signif_for_t_test <- ifelse(
    t_test_p_value < .05 / nrow(dt02), "Yes", "No")
  output <- data.table(
    dt02,
    group_1_mean = signif(group_1_mean, sigfigs),
    group_2_mean = signif(group_2_mean, sigfigs),
    cohen_d = signif(cohen_d, sigfigs),
    t_test_p_value = kim::pretty_round_p_value(t_test_p_value),
    mann_whitney_p_value = kim::pretty_round_p_value(
      mann_whitney_p_value),
    bonferroni_signif_for_t_test = bonferroni_signif_for_t_test)
  return(output)
}
