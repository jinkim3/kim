#' Calculate Cohen's d to accompany a one-sample t-test
#'
#' To run this function, the following package(s) must be installed:
#' Package 'psych' v2.1.9 (or possibly a higher version) by
#' William Revelle (2021),
#' <https://cran.r-project.org/package=psych>
#'
#' @param x a numeric vector containing values whose mean will be calculated
#' @param mu the true mean
#' @examples
#' cohen_d_for_one_sample(x = 1:10, mu = 3)
#' cohen_d_for_one_sample(x = c(1:10, NA, NA), mu = 3)
#' @export
cohen_d_for_one_sample <- function(
  x = NULL, mu = NULL) {
  # check arguments
  if (is.null(x)) {
    stop(paste0(
      "Please provide a numeric vector ",
      "(i.e., an input for the argument 'x')."))
  }
  if (is.null(mu)) {
    stop(paste0(
      "Please provide an input for the true mean ",
      "(i.e., an input for the argument 'mu')."))
  }
  # remove na values
  x_no_na <- stats::na.omit(x)
  num_of_dropped_na_values <- length(x) - length(x_no_na)
  if (num_of_dropped_na_values > 0) {
    message(paste0(
      num_of_dropped_na_values,
      " NA values were removed prior to the analysis."))
  }
  sample_mean <- mean(x_no_na)
  sample_sd <- stats::sd(x_no_na)
  output <- (sample_mean - mu) / sample_sd
  return(output)
}
