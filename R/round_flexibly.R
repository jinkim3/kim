#' Round flexibly
#'
#' Round numbers to a flexible number of significant digits.
#' "Flexible" rounding refers to rounding all numbers to the highest
#' level of precision seen among numbers that would have resulted
#' from the 'signif()' function in base R. The usage examples of
#' this function demonstrate flexible rounding (see below).
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
#'
#' # Example 3
#' # If the input is a character vector, the original input will be returned.
#' round_flexibly(c("a", "b", "c"))
#'
#' # Example 4
#' # If the input is a list (e.g., a data.frame) that contains at least
#' # one numeric vector, the numeric vector element(s) will be rounded
#' # flexibly.
#' round_flexibly(data.frame(a = c(1.2345, 123.45), b = c("a", "b")))
#'
#' # Example 5
#' # If the input is a matrix, all numbers will be rounded flexibly
#' round_flexibly(matrix(
#' c(1.23, 2.345, 3.4567, 4.56789), ncol = 2), sigfigs = 3)
#'
#' @export
round_flexibly <- function(
  x = NULL, sigfigs = 3) {
  # for testing the function
  # x <- c(0.12345, 1234, 0.12, 1.23, .01)
  # x <- data.table(a = letters[1:4], b = letters[2:5])
  # x <- data.table(a = letters[1:4], b = letters[2:5], c = 1:4, d = 5:8)
  # x <- data.frame(a = letters[1:4], b = letters[2:5])
  # x <- list(a = letters[1:4], b = letters[2:5])
  # x <- matrix(c(1.23, 2.345, 3.4567, 4.56789), nrow = 2)

  # convert [the object to round flexibly] into a list
  # check if the input is a list containing numeric vectors
  if (is.list(x)) {
    # numeric elements only
    object_to_round <- Filter(function(x) is.numeric(x), x)
  }
  # check if the input is a numeric vector
  if (is.numeric(x)) {
    # if the input is a matrix, temporarily convert it to a data frame
    if (is.matrix(x)) {
      object_to_round <- as.data.frame(x)
    } else {
      object_to_round <- list(x)
    }
  }
  # if the input is neither a list nor a numeric vector
  if (is.list(x) == FALSE & is.numeric(x) == FALSE) {
    object_to_round <- NULL
  }
  # tell the user there is nothing to round
  if (length(object_to_round) == 0) {
    kim::pm(
      "The input 'x' is not, or does not seem to contain, any numeric ",
      "vector that could be rounded flexibly.")
    return(x)
  }
  # round flexibly each element in the list, object_to_round
  rounded_object <- lapply(object_to_round, function(y) {
    # deal with na
    non_na_values <- y[which(!is.na(y))]
    # first round to significant digits
    nums_sigfig_rounded <- signif(non_na_values, sigfigs)
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
    nums_flexibly_rounded <- round(
      non_na_values, digits = - highest_resolution)
    # replace the non na values with the rounded numbers
    y[which(!is.na(y))] <- nums_flexibly_rounded
    return(y)
  })
  # check if the input is a list containing numeric vectors
  if (is.list(x)) {
    indices_of_elem_to_replace <- which(unlist(lapply(x, is.numeric)))
    for (i in seq_along(indices_of_elem_to_replace)) {
      x[[indices_of_elem_to_replace[i]]] <- rounded_object[[i]]
    }
  }
  # check if the input is a numeric vector
  if (is.numeric(x)) {
    # if the input is a matrix, convert it back to a matrix
    if (is.matrix(x)) {
      x <- as.matrix(as.data.frame(rounded_object))
    } else {
      x <- unlist(rounded_object)
    }
  }
  return(x)
}
