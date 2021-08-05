#' Weighted mean correlation
#'
#' Calculate the weighted mean correlation coefficient for a given
#' correlations and sample sizes.
#' See Field (2001) \doi{10.1037/1082-989X.6.2.161}.
#'
#' @param r a (vector of) correlation coefficient(s)
#' @param n a (vector of) sample size(s)
#' @return the output will be a list of  vector of correlation coefficient(s).
#' @examples
#' weighted_mean_r(r = c(0.2, 0.4), n = c(100, 100))
#' weighted_mean_r(r = c(0.2, 0.4), n = c(100, 20000))
#' @export
# meta analysis function
weighted_mean_r <- function(r = NULL, n = NULL) {
  # return r if there is only one r
  if (length(r) == 1) {
    kim::pm("The input contains only one value of r. ",
            "The output is simply this r.")
    return(r)
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
  # step 9. find p value of z
  z_score <- mean_of_z_sub_r / se_of_mean_of_z_sub_r
  p_value <- 2 * stats::pnorm(q = -abs(z_score))
  # print p
  kim::pm("Weighted mean z's ",
     kim::pretty_round_p_value(p_value, include_p_equals = TRUE))
  # step 10. z to r
  overall_r <- kim::z_to_r_transform(mean_of_z_sub_r)
  # output
  output <- c(overall_r, p_value)
  names(output) <- c("weighted_mean_r", "p_value")
  return(output)
}
