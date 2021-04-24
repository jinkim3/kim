#' Round flexibly
#'
#' Round numbers to a flexible number of significant digits.
#' "Flexible" rounding refers to rounding all numbers to the highest
#' level of precision seen among numbers that would have resulted
#' from the 'signif()' function in base R. The examples of this function
#' demonstrate flexible rounding.
#'
#' @param x a numeric vector
#' @param sigfigs number of significant digits to flexibly round to.
#' By default, \code{sigfigs = 3}.
#' @return the output will be a numeric vector with values rounded
#' to the highest level of precision seen among numbers that result
#' from the 'signif()' function in base R.
#' @examples
#' # Example 1
#' # First, observe results from the 'signif' function:
#' c(0.00012345, pi)
#' signif(c(0.00012345, pi), 3)
#' # In the result above, notice how info is lost on some digits
#' # (e.g., 3.14159265 becomes 3.140000).
#' # In contrast, flexible rounding retains the lost info in the digits
#' round_flexibly(x = c(0.00012345, pi), sigfigs = 3)
#'
#' # Example 2
#' # Again, first observe results from the 'signif' function:
#' c(0.12345, 1234, 0.12, 1.23, .01)
#' signif(c(0.12345, 1234, 0.12, 1.23, .01), 3)
#' # In the result above, notice how info is lost on some digits
#' # (e.g., 1234 becomes 1230.000).
#' # In contrast, flexible rounding retains the lost info in the digits.
#' # Specifically, in the example below, 0.12345 rounded to 3 significant
#' # digits (default) is signif(0.12345, 3) = 0.123 (3 decimal places).
#' # Because this 3 decimal places is the highest precision seen among
#' # all numbers, all other numbers will also be rounded to 3 decimal places.
#' round_flexibly(
#' c(0.12345, 1234, 0.12, 1.23, .01))
#' @export
round_flexibly <- function(
  x = NULL, sigfigs = 3) {
  # check if numeric
  if (!is.numeric(x)) {
    stop(message("The input for 'x' is not a numeric vector."))
  }
  # first round to significant digits
  nums_sigfig_rounded <- signif(x, sigfigs)
  # count the digits in the first part of the scientific
  # notation of individual numbers
  num_digits_in_sci_notn_pt_1 <- nchar(
    gsub("\\.", "", vapply(nums_sigfig_rounded, function(j) {
      gsub("(^.*)e.*$", "\\1", format(j, scientific = TRUE))
    }, character(1L))))
  # extract the precision level from the second part of the
  # scientific notation
  preci_lvl_from_sci_notn_pt_2 <- as.numeric(
    gsub("^.*e(.*$)", "\\1", format(
      nums_sigfig_rounded, scientific = TRUE)))
  # what is the highest resolution (or precision) among the numbers?
  highest_resolution <- min(
    preci_lvl_from_sci_notn_pt_2 - (num_digits_in_sci_notn_pt_1 - 1))
  # round regularly to the digit place with the highest level of precision
  nums_flexibly_rounded <- round(x, digits = - highest_resolution)
  return(nums_flexibly_rounded)
}
