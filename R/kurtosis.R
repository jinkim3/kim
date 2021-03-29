#' Kurtosis
#'
#' Calculate kurtosis of the sample using a formula for either the
#' (1) biased estimator or (2) an unbiased estimator of the
#' population kurtosis. Formulas were taken from DeCarlo (1997),
#' \doi{10.1037/1082-989X.2.3.292}
#'
#' @param vector a numeric vector
#' @param unbiased logical. If \code{unbiased = TRUE}, the unbiased
#' estimate of the population kurtosis will be calculated.
#' If \code{unbiased = FALSE}, the biased estimate of the population
#' kurtosis will be calculated. By default, \code{unbiase = TRUE}.
#' @return a numeric value, i.e., kurtosis of the given vector
#' @examples
#' # calculate the unbiased estimator (e.g., kurtosis value that
#' # Excel 2016 will produce)
#' kim::kurtosis(c(1, 2, 3, 4, 5, 10))
#' # calculate the biased estimator (e.g., kurtosis value that
#' # R Package 'moments' will produce)
#' kim::kurtosis(c(1, 2, 3, 4, 5, 10), unbiased = FALSE)
#' # compare with kurtosis from 'moments' package
#' moments::kurtosis(c(1, 2, 3, 4, 5, 10))
#' @export
kurtosis <- function(
  vector = NULL,
  unbiased = TRUE) {
  # throw an error if no vector is given
  if (is.null(vector)) {
    stop("Please enter a vector for which to calculate kurtosis.")
  }
  # throw an error if vector is not numeric
  if (!is.numeric(vector)) {
    stop("Please enter a numeric vector for which to calculate kurtosis.")
  }
  # throw an error if the input for `unbiased` is not a logical value
  if (!(is.logical(unbiased))) {
    stop(paste0(
      "The input for the argument `unbiased` ",
      "must be either TRUE or FALSE."))
  }
  # remove NA values
  x <- vector[!is.na(vector)]
  # calculate stats
  x_bar <- mean(x)
  s <- stats::sd(x)
  n <- length(x)
  # calculate the unbiased estimator
  if (unbiased == TRUE) {
    unbiased_kurtosis <- (n * (n + 1) * sum((x - x_bar) ^ 4)) /
      ((n - 1) * (n - 2) * (n - 3) * (sum((x - x_bar) ^ 2) / (n - 1)) ^ 2) -
      (3 * (n - 1) ^ 2) / ((n - 2) * (n - 3))
    return(unbiased_kurtosis)
  }
  # calculate the biased estimator
  if (unbiased == FALSE) {
    biased_kurtosis <- (sum((x - x_bar) ^ 4) / n) /
      (sum((x - x_bar) ^ 2) / n) ^ 2
    return(biased_kurtosis)
  }
}
