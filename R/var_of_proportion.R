#' Variance of a proportion
#'
#' Calculate the variance of a proportion
#' Anderson and Finn (1996, p. 364, ISBN: 978-1-4612-8466-6)
#'
#' @param p proportion; a value between 0 and 1
#' @param n sample size; number of observations used to calculate
#' the proportion
#' @examples
#' var_of_proportion(p = 0.56, n = 400)
#' var_of_proportion(p = 0.5, n = 100)
#' var_of_proportion(p = 0.4, n = 50)
#' @export
var_of_proportion <- function(p = NULL, n = NULL) {
  # check that the proportion is in the expected range
  if (p < 0) {
    stop("Please check your input. A proportion cannot be less than 0.")
  }
  if (p > 1) {
    stop("Please check your input. A proportion cannot be greater than 1.")
  }
  # check the n
  if (is.null(n)) {
    stop("Please provide an input for the argument `n`.")
  }
  return(p * (1 - p) / n)
}
