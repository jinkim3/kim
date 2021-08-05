#' Weighted z
#'
#' Calculate the weighted z (for calculating weighted mean correlation).
#' See p. 231 of the book Hedges & Olkin (1985),
#' Statistical Methods for Meta-Analysis (ISBN: 0123363802).
#'
#' @param z a vector of z values
#' @param n a vector of sample sizes which will be used to calculate the
#' weights, which in turn will be used to calculate the weighted z.
#' @return the output will be a weighted z value.
#' @examples
#' weighted_z(1:3, c(100, 200, 300))
#' weighted_z(z = c(1:3, NA), n = c(100, 200, 300, NA))
#' @export
weighted_z <- function(z = NULL, n = NULL) {
  # check vector lengths
  if (length(z) <= 1) {
    stop("The input for z must be a vector of 2 or more values.")
  }
  if (length(n) <= 1) {
    stop("The input for n must be a vector of 2 or more values.")
  }
  if (length(z) != length(n)) {
    stop("The inputs for z and n must be of the same length.")
  }
  # calculate weights
  weights <- (n - 3) / sum(n - 3, na.rm = TRUE)
  # return weighted z
  weighted_z <- sum(z * weights, na.rm = TRUE)
  return(weighted_z)
}
