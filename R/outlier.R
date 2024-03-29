#' Outlier
#'
#' Return outliers in a vector
#'
#' @param x a numeric vector
#' @param iqr a nonnegative constant by which interquartile range (IQR)
#' will be multiplied to build a "fence," outside which observations
#' will be considered outliers. For example, if \code{iqr = 1.5},
#' IQR * 1.5 will be the "fence" outside which observations will be
#' considered to be outliers. By default, \code{iqr = 1.5}.
#' @param na.rm logical. \code{na.rm} argument to be passed onto the
#' 'quantile' function in the 'stats' package. If true, any NA and NaN's
#' are removed from x before the quantiles are computed.
#' @param unique_outliers logical. If \code{unique_outliers = TRUE},
#' the function will return the unique outlier values.
#' If \code{unique_outliers = FALSE}, the function will return all
#' the outlier values in the vector \code{x}.
#' By default, \code{unique_outliers = FALSE}.
#' @param type \code{type} argument to be passed onto the 'quantile'
#' function in the 'stats' package. An integer between 1 and 9 selecting
#' one of the nine quantile algorithms detailed below to be used.
#' Type '?stats::quantile' for details. By default, \code{type = 7}
#' @return the output will be a numeric vector with outliers removed.
#' @examples
#' # Example 1
#' outlier(c(1:10, 100))
#' # The steps below show how the outlier, 100, was obtained
#' # v1 is the vector of interest
#' v1 <- c(1:10, 100)
#' # quantile
#' stats::quantile(v1)
#' # first and third quartiles
#' q1 <- stats::quantile(v1, 0.25)
#' q3 <- stats::quantile(v1, 0.75)
#' # interquartile range
#' interquartile_range <- unname(q3 - q1)
#' # fence, using the default 1.5 as the factor to multiply the IQR
#' cutoff_low <- unname(q1 - 1.5 * interquartile_range)
#' cutoff_high <- unname(q3 + 1.5 * interquartile_range)
#' v1[v1 < cutoff_low | v1 > cutoff_high]
#' @export
outlier <- function(
  x = NULL, iqr = 1.5, na.rm = TRUE, type = 7, unique_outliers = FALSE) {
  # check if numeric
  if (!is.numeric(x)) {
    stop(message("The input for 'x' is not a numeric vector."))
  }
  # remove na values
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }
  # quartiles
  q1 <- stats::quantile(x, 0.25, type = type)
  q3 <- stats::quantile(x, 0.75, type = type)
  # interquartile range
  interquartile_range <- q3 - q1
  # cutoff points
  cutoff_low <- q1 - iqr * interquartile_range
  cutoff_high <- q3 + iqr * interquartile_range
  # outliers
  outliers <- x[x < cutoff_low | x > cutoff_high]
  # unique outlier values?
  if (unique_outliers == TRUE) {
    outliers <- kim::su(outliers)
  }
  return(outliers)
}
