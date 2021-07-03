#' Compare dependent correlations
#'
#' Compares whether two dependent correlations from the same sample are
#' significantly different each other.
#'
#' Suppose that Variables A, B, and C are measured from a group of subjects.
#' This function tests whether A is related to B differently than to C.
#' Put differently, this function tests H0: r(A, B) = r(A, C)
#'
#' For more information on formulas used in this function, please refer to
#' Steiger (1980) and Chen & Popovich (2002)
#'
#' @param data a data object (a data frame or a data.table)
#' @param var_1_name name of the variable whose correlations with two other
#' variables will be compared.
#' @param var_2_name name of the first of the two variables whose
#' correlations with \code{var_1_name} will be compared.
#' @param var_3_name name of the second of the two variables whose
#' correlations with \code{var_1_name} will be compared.
#' @param one_tailed logical. Should the p value based on a one-tailed
#' t-test? (default = FALSE)
#' @param round_r number of decimal places to which to round
#' correlation coefficients (default = 2)
#' @param round_p number of decimal places to which to round
#' p-values (default = 3)
#' @param round_t number of decimal places to which to round the
#' t-statistic (default = 2)
#' @param print_summary logical. Should the summary be printed?
#' (default = TRUE)
#' @return the output will be a summary of the test comparing two dependent
#' correlations
#' @examples
#' compare_dependent_rs(
#' data = mtcars, var_1_name = "mpg", var_2_name = "hp", var_3_name = "wt")
#' @export
#' @import data.table
compare_dependent_rs <- function(
  data = NULL,
  var_1_name = NULL,
  var_2_name = NULL,
  var_3_name = NULL,
  one_tailed = FALSE,
  round_r = 3,
  round_p = 3,
  round_t = 2,
  print_summary = TRUE
  ) {
  # deal with na values
  dt <- stats::na.omit(data.frame(
    j = data[[var_1_name]],
    h = data[[var_2_name]],
    k = data[[var_3_name]]))
  # estimate correlations
  r_jk_results <- stats::cor.test(dt$j, dt$k)
  r_jh_results <- stats::cor.test(dt$j, dt$h)
  r_kh_results <- stats::cor.test(dt$k, dt$h)
  # correlation only
  r_jk <- unname(r_jk_results$estimate)
  r_jh <- unname(r_jh_results$estimate)
  r_kh <- unname(r_kh_results$estimate)
  # p values
  r_jk_p_value <- r_jk_results$p.value
  r_jh_p_value <- r_jh_results$p.value
  # n
  n <- nrow(dt)
  # # formula from chen popovich
  # r_jk <- 0.5
  # r_jh <- 0.2
  # r_kh <- 0.3
  # n <- 103
  # abs_r <- 1 - r_jk ^ 2 - r_jh ^ 2 - r_kh ^ 2 + 2 * r_jk * r_jh * r_kh
  # r_bar <- (r_jk + r_jh) / 2
  # t2 <- (r_jk - r_jh) * sqrt((n - 1) * (1 + r_kh) / (
  #   2 * ((n - 1) / (n - 3)) * abs_r + r_bar ^ 2 * (1 - r_kh) ^ 3))
  # t2
  # formula from steiger
  # r_jk <- 0.4
  # r_jh <- 0.5
  # r_kh <- 0.1
  # n <- 103
  abs_big_r <- 1 - r_jk ^ 2 - r_jh ^ 2 - r_kh ^ 2 + 2 * r_jk * r_jh * r_kh
  small_r_bar <- (r_jk + r_jh) / 2
  t_diff <- (r_jk - r_jh) * sqrt(
    (n - 1) * (1 + r_kh) /
      (2 * (n - 1) / (n - 3) * abs_big_r +
         small_r_bar ^ 2 * (1 - r_kh) ^ 3))
  # df for t test for difference
  df_diff <- n - 3
  # p value of difference, from the t test
  if (one_tailed == TRUE) {
    p_diff <- stats::pt(q = -abs(t_diff), df = df_diff)
  } else {
    p_diff <- 2 * stats::pt(q = -abs(t_diff), df = df_diff)
  }
  # test text
  test_text <- paste0(
    "A ",
    ifelse(one_tailed == TRUE, "one-tailed", "two-tailed"),
    " t-test revealed that ")
  # sig text
  sig_text <- ifelse(
    p_diff < 0.05, "significantly different",
    "not significantly different")
  # rounded rs
  r_jk_rounded <- round(r_jk, round_r)
  r_jh_rounded <- round(r_jh, round_r)
  # rounded p values for the two correlations
  r_jk_p_value_rounded <- kim::pretty_round_p_value(
    r_jk_p_value, include_p_equals = TRUE,
    round_digits_after_decimal = round_p)
  r_jh_p_value_rounded <- kim::pretty_round_p_value(
    r_jh_p_value, include_p_equals = TRUE,
    round_digits_after_decimal = round_p)
  # rounded p value for the test of difference in rs
  p_diff_rounded <- kim::pretty_round_p_value(
    p_diff, include_p_equals = TRUE,
    round_digits_after_decimal = round_p)
  # rounded t
  t_diff_rounded <- round(t_diff, round_t)
  # print summary
  # correlation 1
  kim::pm(
    "Correlation between ", var_1_name, " and ", var_2_name, ": ",
    "r = ", r_jk_rounded, ", ", r_jk_p_value_rounded)
  # correlation 2
  kim::pm(
    "Correlation between ", var_1_name, " and ", var_3_name, ": ",
    "r = ", r_jh_rounded, ", ", r_jh_p_value_rounded)
  # p value
  kim::pm(
    test_text,
    "the two correlations, ", r_jk_rounded, " and ",
    r_jh_rounded, ", are ", sig_text,
    ", t = ", t_diff_rounded, " ", p_diff_rounded)
  # output table
  description <- c(
    paste0("r(", var_1_name, ", ", var_2_name, ")"),
    paste0("p-value of r(", var_1_name, ", ", var_2_name, ")"),
    paste0("r(", var_1_name, ", ", var_3_name, ")"),
    paste0("p-value of r(", var_1_name, ", ", var_3_name, ")"),
    "t-stat for difference in rs",
    "p-value for difference in rs")
  value <- c(
    r_jk_rounded,
    round(r_jk_p_value, round_r),
    r_jh_rounded,
    round(r_jh_p_value, round_r),
    t_diff_rounded,
    round(p_diff, round_p))
  output <- data.table::data.table(
    description, value)
  invisible(output)
}
