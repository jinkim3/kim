#' Weighted mean correlation
#'
#' Calculate the weighted mean correlation coefficient for a given
#' correlations and sample sizes.
#' See Field (2001) \doi{10.1037/1082-989X.6.2.161}.
#'
#' @param r a (vector of) correlation coefficient(s)
#' @param n a (vector of) sample size(s)
#' @param sigfigs number of significant digits to round to (default = 3)
#' @param ci size of the confidence interval. Input can be any value
#' less than 1 and greater than or equal to 0. By default, \code{ci = 0.95}.
#' If \code{ci = TRUE}, the default value of 0.95 will be used. If \code{
#' ci = FALSE}, no confidence interval will be estimated.
#' @return the output will be a list of  vector of correlation coefficient(s).
#' @examples
#' weighted_mean_r(r = c(0.2, 0.4), n = c(100, 100))
#' weighted_mean_r(r = c(0.2, 0.4), n = c(100, 20000))
#' # example consistent with using MedCalc
#' weighted_mean_r(
#' r = c(0.51, 0.48, 0.3, 0.21, 0.6, 0.46, 0.22, 0.25),
#' n = c(131, 129, 155, 121, 111, 119, 112, 145))
#' @export
# meta analysis function
weighted_mean_r <- function(
  r = NULL,
  n = NULL,
  ci = 0.95,
  sigfigs = 3) {
  # return r if there is only one r
  if (length(r) == 1) {
    kim::pm("The input contains only one value of r. ",
            "The output is simply this r.")
    return(r)
  }
  # check valid ci input
  if (ci == TRUE) {
    ci <- 0.95
  }
  if (ci < 0 | ci >= 1) {
    stop("The input for 'ci' must be in the range [0, 1).")
  }
  # step 1. r to z
  z_sub_r_sub_i <- kim::fisher_z_transform(r)
  # step 2. obtain the Q stat
  q_stat <- kim::q_stat_test_homo_r(z = z_sub_r_sub_i, n = n)
  # step 3. obtain k
  k <- length(n)
  # step 4. obtain the constant c
  c <- sum((n - 3), na.rm = TRUE) - (
    sum((n - 3) ^ 2, na.rm = TRUE) /
      sum((n - 3), na.rm = TRUE))
  # step 5. obtain tau squared
  # ensure tau squared is not negative
  tau_squared <- max(0, (q_stat - (k - 1)) / c)
  # step 6. obtain weights
  v_sub_i <- 1 / (n - 3)
  w_sub_i <- 1 / (v_sub_i + tau_squared)
  random_effect_model_weights <- w_sub_i
  # step 7. weighted mean z
  mean_of_z_sub_r <-
    sum(random_effect_model_weights * z_sub_r_sub_i, na.rm = TRUE) /
    sum(random_effect_model_weights, na.rm = TRUE)
  # step 8. se of weighted mean z
  se_of_mean_of_z_sub_r <-
    sqrt(1 / sum(random_effect_model_weights, na.rm = TRUE))
  # step 9. put a ci around the weighted mean z
  # find critical values
  cv <- c((1 - ci) / 2, 1 - (1 - ci) / 2)
  z_ci_limits <- mean_of_z_sub_r + se_of_mean_of_z_sub_r * stats::qnorm(cv)
  # step 10. find p value of z
  z_score <- mean_of_z_sub_r / se_of_mean_of_z_sub_r
  p_value <- 2 * stats::pnorm(q = -abs(z_score))
  # step 11. z to r
  overall_r <- kim::z_to_r_transform(mean_of_z_sub_r)
  overall_r_ci_limits <- kim::z_to_r_transform(z_ci_limits)
  # print r, p, and ci
  kim::pm(
    "Weighted mean r: ",
    signif(overall_r, sigfigs), ", ",
    kim::pretty_round_p_value(p_value, include_p_equals = TRUE), ", ",
    kim::round_flexibly(ci * 100, sigfigs), "% CI = [",
    paste0(kim::round_flexibly(overall_r_ci_limits, sigfigs), collapse = ", "), "]")
  # output
  output <- c(overall_r, p_value, overall_r_ci_limits)
  names(output) <- c("weighted_mean_r", "p_value", "ci_ll", "ci_ul")
  return(output)
}
