#' Variance of a percentage
#'
#' Calculate the variance of a percentage.
#' See Fowler, Jr. (2014, p. 34, ISBN: 978-1-4833-1240-8)
#'
#' @param percent percentage; a value between 0 and 100
#' @param n sample size; number of observations used to calculate
#' the percentage
#' @examples
#' var_of_percentage(percent = 40, n = 50)
#' var_of_percentage(percent = 50, n = 10)
#' @export
var_of_percentage <- function(percent = NULL, n = NULL) {
  # check that the percentage is in the expected range
  if (percent < 0) {
    stop(paste0(
      "Please check your input.",
      "\nA percentage for this function cannot be less than 0."))
  }
  if (percent > 100) {
    stop(paste0(
      "Please check your input.",
      "\nA percentage for this function cannot be greater than 100."))
  }
  # check the n
  if (is.null(n)) {
    stop("Please provide an input for the argument `n`.")
  }
  # convert percentage to a proportion
  p <- percent / 100
  return((p * (1 - p) / n) * 100)
}
