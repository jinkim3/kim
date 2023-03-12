#' Geometric mean
#'
#' Calculate the geometric mean of a numeric vector
#'
#' @param x a numeric vector
#' @param zero_or_neg_convert_to the value to which zero or negative
#' values will be converted to. If \code{zero_or_neg_convert_to == NA},
#' zero or negative values will be converted to NA values and thus be
#' excluded when calculating the geometric mean. (default = NA)
#' @examples
#' \dontrun{
#' geomean(c(1, 4))
#' geomean(c(1, 100))
#' geomean(c(1, 100, NA))
#' geomean(c(1, 100, NA, 0, -1, -2))
#' geomean(
#' x = c(1, 100, NA, 0, -1, -2),
#' zero_or_neg_convert_to = 1)
#' geomean(c(1, 100, NA, 1, 1, 1))
#' }
#' @export
geomean <- function(
    x = NULL, zero_or_neg_convert_to = NA) {
  # ensure the vector is numeric
  if (is.numeric(x) == FALSE) {
    stop("The input x must be a numeric vector.")
  }
  # count zero or negative values
  count_of_zero <- sum(x == 0, na.rm = TRUE)
  count_of_neg <- sum(x < 0, na.rm = TRUE)
  if (count_of_zero == 0 & count_of_neg == 0) {
    output <- exp(mean(log(x), na.rm = TRUE))
  } else {
    if (is.na(zero_or_neg_convert_to)) {
      kim::pm(
        "Zeros (count: ",
        count_of_zero,
        ") and negative values (count: ",
        count_of_neg,
        ")\nwere excluded before calculating the geomtric mean below:")
    } else {
      kim::pm(
        "Zeros (count: ",
        count_of_zero,
        ") and negative values (count: ",
        count_of_neg,
        ")\nwere converted to ",
        zero_or_neg_convert_to,
        " before calculating the geomtric mean below:")
    }
    x[x <= 0] <- zero_or_neg_convert_to
    output <- exp(mean(log(x), na.rm = TRUE))
  }
  return(output)
}
