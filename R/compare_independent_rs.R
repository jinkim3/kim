#' Compare independent correlations
#'
#' Compares whether two correlations from two independent samples are
#' significantly different each other.
#' See Field et al. (2012, ISBN: 978-1-4462-0045-2).
#'
#' @param r1 correlation in the first sample
#' @param n1 size of the first sample
#' @param r2 correlation in the second sample
#' @param n2 size of the first sample
#' @param one_tailed logical. Should the p value based on a one-tailed
#' t-test? (default = FALSE)
#' @param round_p (only for displaying purposes) number of decimal
#' places to which to round the p-value (default = 3)
#' @param round_z_diff (only for displaying purposes) number of
#' decimal places to which to round the z-score (default = 2)
#' @param round_r (only for displaying purposes) number of
#' decimal places to which to round correlation coefficients (default = 2)
#' @param print_summary logical. Should the summary be printed?
#' (default = TRUE)
#' @param output_type type of the output. If \code{output_type = "z"},
#' the function's output will be the z-score of the difference between
#' the two correlations. If \code{output_type = "p"}, the function's
#' output will be the p-value associated with the z-score of the
#' difference between the two correlations. By default,
#' \code{output_type = NULL}, and the function will not return any
#' value other than the printed summary.
#' @return the output will be the results of a test comparing two
#' independent correlations.
#' @examples
#' compare_independent_rs(r1 = .1, n1 = 100, r2 = .2, n2 = 200)
#' compare_independent_rs(
#' r1 = .1, n1 = 100, r2 = .2, n2 = 200, one_tailed = TRUE)
#' compare_independent_rs(r1 = .506, n1 = 52, r2 = .381, n2 = 51)
#' @export
#' @import data.table
compare_independent_rs <- function(
  r1 = NULL,
  n1 = NULL,
  r2 = NULL,
  n2 = NULL,
  one_tailed = FALSE,
  round_p = 3,
  round_z_diff = 2,
  round_r = 2,
  print_summary = TRUE,
  output_type = NULL
  ) {
  # r to z
  z_from_r1 <- kim::fisher_z_transform(r1)
  z_from_r2 <- kim::fisher_z_transform(r2)
  # calculate the z score of the differences between the correlations
  z_diff <- (z_from_r1 - z_from_r2) / sqrt(
    1 / (n1 - 3) + 1 / (n2 - 3))
  # find p value of z
  if (one_tailed == TRUE) {
    p_value <- stats::pnorm(q = -abs(z_diff))
  } else {
    p_value <- 2 * stats::pnorm(q = -abs(z_diff))
  }
  # sig text
  sig_text <- ifelse(
    p_value < 0.05, "significantly different",
    "not significantly different")
  if (print_summary == TRUE) {
    kim::pm(
      "The two correlations, ", kim::pretty_round_r(r1, round_r),
      " and ", kim::pretty_round_r(r2, round_r), ", are ", sig_text,
      ", ", ifelse(one_tailed == TRUE, "one-tailed", "two-tailed"),
      " z = ", round(z_diff, round_z_diff), ", ",
      kim::pretty_round_p_value(
        p_value, round_p, include_p_equals = TRUE))
  }
  # output
  if (!is.null(output_type)) {
    if (output_type == "z") {
      return(z_diff)
    } else if (output_type == "p") {
      return(p_value)
    }
  }
}
