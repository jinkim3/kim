#' Variance of a proportion
#'
#' Calculate the variance of a proportion.
#' See Anderson and Finn (1996, p. 364, ISBN: 978-1-4612-8466-6)
#'
#' @param p a vector of proportions; each of the proportion values
#' must be between 0 and 1
#' @param n a vector of sample sizes; number of observations used
#' to calculate each of the percentage values
#' @examples
#' var_of_proportion(p = 0.56, n = 400)
#' var_of_proportion(p = 0.5, n = 100)
#' var_of_proportion(p = 0.4, n = 50)
#' var_of_proportion(p = c(0.5, 0.9), n = c(100, 200))
#' @export
var_of_proportion <- function(p = NULL, n = NULL) {
  # check that the proportion is in the expected range
  if (any(p < 0)) {
    stop("Please check your input. A proportion cannot be less than 0.")
  }
  if (any(p > 1)) {
    stop("Please check your input. A proportion cannot be greater than 1.")
  }
  # check the n
  if (is.null(n)) {
    stop("Please provide an input for the argument `n`.")
  }
  return(p * (1 - p) / n)
}
