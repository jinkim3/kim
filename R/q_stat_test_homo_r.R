#' Q statistic for testing homogeneity of correlations
#'
#' Calculate the Q statistic to test for homogeneity of correlation
#' coefficients.
#' See p. 235 of the book Hedges & Olkin (1985),
#' Statistical Methods for Meta-Analysis (ISBN: 0123363802).
#'
#' @param z a vector of z values
#' @param n a vector of sample sizes which will be used to calculate the
#' weights, which in turn will be used to calculate the weighted z.
#' @return the output will be a weighted z value.
#' @examples
#' q_stat_test_homo_r(1:3, c(100, 200, 300))
#' q_stat_test_homo_r(z = c(1:3, NA), n = c(100, 200, 300, NA))
#' @export
q_stat_test_homo_r <- function(z = NULL, n = NULL) {
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
  # calculate the weighted z
  weighted_z <- kim::weighted_z(z = z, n = n)
  # calculate and return the q stat
  q_stat <- sum((n - 3) * (z - weighted_z) ^ 2, na.rm = TRUE)
  return(q_stat)
}
