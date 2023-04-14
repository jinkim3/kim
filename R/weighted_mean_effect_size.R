#' Estimate the mean effect size in a meta analysis
#'
#' Estimate the mean effect size in a meta analysis, as illustrated
#' in Borenstein et al. (2009, pp. 73-74, ISBN: 978-0-470-05724-7)
#'
#' @param effect_sizes effect sizes (e.g., standardized mean differences)
#' @param effect_size_variances within-study variances
#' @param ci width of the confidence interval (default = 0.95)
#' @param one_tailed logical. If \code{one_tailed = FALSE}, a two-tailed
#' p-value will be calculated. If \code{one_tailed = TRUE}, a one-tailed
#' p-value will be calculated (default = FALSE)
#' @param random_vs_fixed If \code{random_vs_fixed = "random"},
#' the summary effect will be calculated under the random-effects model
#' (default = "random").
#' @examples
#' \dontrun{
#' weighted_mean_effect_size(
#' effect_sizes = c(1, 2), effect_size_variances = c(3, 4))
#' weighted_mean_effect_size(
#' effect_sizes = c(0.095, 0.277, 0.367, 0.664, 0.462, 0.185),
#' effect_size_variances = c(0.033, 0.031, 0.050, 0.011, 0.043, 0.023))
#' }
#' @export
weighted_mean_effect_size <- function(
    effect_sizes = NULL,
    effect_size_variances = NULL,
    ci = 0.95,
    one_tailed = FALSE,
    random_vs_fixed = "random") {
  # check inputs ----
  if (is.null(effect_sizes)) {
    stop("Please provide an input for the 'effect_sizes' argument.")
  }
  if (is.null(effect_size_variances)) {
    stop("Please provide an input for the 'effect_size_variances' argument.")
  }
  if (length(effect_sizes) != length(effect_size_variances)) {
    stop("The effect sizes and their variances are of different lengths.")
  }
  if (fixed_vs_random != "random") {
    stop(paste0(
      "The current version of the function only supports calculations",
      " under the random-effects model."))
  }
  # tau-squared which is the between-studies variance
  tau_squared <- kim::tau_squared(
    effect_sizes = effect_sizes,
    effect_size_variances = effect_size_variances)
  # vyi = within-study variance for study i plus
  # the between-studies variance (tau-squared)
  vyi <- effect_size_variances + tau_squared
  # weights = weight assigned to each study
  weights <- 1 / vyi
  # "summary effect" = weighted mean of effect sizes
  summary_effect <- sum(weights * effect_sizes) / sum(weights)
  # variance of the summary effect
  var_of_summary_effect <- 1 / sum(weights)
  # standard error of summary effect
  se_of_summary_effect <- sqrt(var_of_summary_effect)
  # critical values for estimating the confidence interval
  cv <- c((1 - ci) / 2, 1 - (1 - ci) / 2)
  ci_of_summary_effect <-
    summary_effect + se_of_summary_effect * stats::qnorm(cv)
  # ll stands for lower limit; ul for upper limit
  summary_effect_ci_ll <- min(ci_of_summary_effect)
  summary_effect_ci_ul <- max(ci_of_summary_effect)
  # z value to test the null hypo that the mean effect is zero
  z <- summary_effect / se_of_summary_effect
  # p value for the z test above
  # z <- 1.24 z <- -1.24 z <- -0.77 z <- 1.87 z <- 0 z = 0.00001
  if (z > 0) {
    p <- stats::pnorm(q = z, lower.tail = FALSE)
    p_label <- "p_right_tailed"
  } else {
    p <- stats::pnorm(q = z, lower.tail = TRUE)
    p_label <- "p_left_tailed"
  }
  # one-tailed p
  if (one_tailed != TRUE) {
    p <- p * 2
    p_label <- "p_two_tailed"
  }
  # output ----
  output <- c(
    summary_effect = summary_effect,
    summary_effect_ci_ll = summary_effect_ci_ll,
    summary_effect_ci_ul = summary_effect_ci_ul,
    var_of_summary_effect = var_of_summary_effect,
    p)
  names(output)[length(output)] <- p_label
  return(output)
}
