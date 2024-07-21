#' Pretty round p-value
#'
#' Round p-values to the desired number of decimals and remove
#' leading 0s before the decimal.
#'
#' @param p_value_vector one number or a numeric vector
#' @param include_p_equals if \code{TRUE}, output will be a string of
#' mathematical expression including "p", e.g., "p < .01" (default = FALSE)
#' @param round_digits_after_decimal how many digits after the decimal
#' point should the p-value be rounded to?
#' @return the output will be a character vector with p values, e.g.,
#' a vector of strings like "< .001" (or "p < .001").
#' @examples
#' pretty_round_p_value(0.00001)
#' pretty_round_p_value(0.00001, round_digits_after_decimal = 4)
#' pretty_round_p_value(0.00001, round_digits_after_decimal = 5)
#' # WARNING: the line of code below adding precision that may be unwarranted
#' pretty_round_p_value(0.00001, round_digits_after_decimal = 6)
#' pretty_round_p_value(
#'   p_value_vector = 0.049,
#'   round_digits_after_decimal = 2, include_p_equals = FALSE)
#' pretty_round_p_value(c(0.0015, 0.0014, 0.0009), include_p_equals = TRUE)
#' @export
pretty_round_p_value <- function(
  p_value_vector = NULL,
  round_digits_after_decimal = 3,
  include_p_equals = FALSE) {
  # check if numeric
  if (is.numeric(p_value_vector) == FALSE) {
    stop(paste0("The input for p_value_vector is not a numeric vector."))
  }
  # check if negative values exist
  if (any(p_value_vector[!is.na(p_value_vector)] < 0)) {
    stop(paste0("The input for p_value_vector contains a negative ",
    "p-value, which should not possible."))
  }
  # check if p-values are between 0 and 1
  if (any(p_value_vector[!is.na(p_value_vector)] > 1)) {
    stop(paste0("The input for p_value_vector contains a p-value ",
                "greater than 1, which should not possible."))
  }
  # proceed if p_value_vector is numeric
  if (is.numeric(p_value_vector)) {
    # include the p and equal sign
    if (include_p_equals == TRUE) {
      output <- ifelse(
        p_value_vector < 1 / (10 ^ round_digits_after_decimal),
        paste0("p < ", gsub(
          "^0\\.", ".",
          as.character(format(
            1 / (10 ^ round_digits_after_decimal), scientific = FALSE)))),
        paste0("p = ", sub("^0?", "", sprintf(
          paste0("%.", round_digits_after_decimal, "f"),
          p_value_vector))))
      return(output)
    }
    # exclude the p and equal sign
    output <- ifelse(
      p_value_vector < 1 / (10 ^ round_digits_after_decimal),
      paste0("p < ", gsub(
        "^0\\.", ".",
        as.character(format(
          1 / (10 ^ round_digits_after_decimal), scientific = FALSE)))),
      sub("^0?", "", sprintf(
        paste0("%.", round_digits_after_decimal, "f"),
        p_value_vector)))
    # return the pretty p values
    return(output)
  } else if (is.character(p_value_vector)) {
    stop(paste0(
      "The vector, p_value_vector, is a character",
      " vector, rather than a numeric vector"
    ))
  } else {
    stop(paste0(
      "The vector, p_value_vector, is neither",
      " a character vector nor a numeric vector"
    ))
  }
}
