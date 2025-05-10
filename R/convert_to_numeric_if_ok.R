#' Convert a vector to numeric if appropriate
#'
#' Check whether a vector contains only values that can be
#' interpreted as numbers, and convert to numeric if so.
#'
#' @param x a vector
#' @param notify_failure logical. Should a conversion failure be notified?
#' (default = TRUE).
#' @param notify_success logical. Should a conversion failure be notified?
#' (default = TRUE)
#' @examples
#' \donttest{
#' # conversion should succeed
#' convert_to_numeric_if_ok(c(1, "2"))
#' convert_to_numeric_if_ok(c("1", "2"))
#' convert_to_numeric_if_ok(
#' c("1", "2", "Inf", "-Inf", " 123 ", "-1.2", "+0.5", "1e-2"))
#'
#' # conversion should fail
#' convert_to_numeric_if_ok(c(1, 2, "a"))
#' # A vector of diverse test cases to evaluate numeric conversion handling:
#' crazy_vec <- c(
#' "123",        # valid integer
#' " 456 ",      # valid number with leading/trailing whitespace
#' "-78.9",      # valid negative decimal
#' "+0.5",       # valid number with explicit plus sign
#' "1e-3",       # valid scientific notation
#' "1.2.3",      # invalid: multiple decimal points
#' "abc123",     # invalid: contains letters
#' "$100",       # invalid: contains currency symbol
#' "NaN",        # valid: special numeric value (Not a Number)
#' "Inf",        # valid: positive infinity
#' "-Inf",       # valid: negative infinity
#' "TRUE",       # invalid: logical string, not numeric
#' "",           # invalid: empty string
#' " ",          # invalid: whitespace only
#' "\t\n",       # invalid: tab and newline characters
#' NA            # valid: actual missing value
#' )
#' convert_to_numeric_if_ok(crazy_vec)
#' }
#' @export
#' @import data.table
convert_to_numeric_if_ok <- function(
    x,
    notify_failure = TRUE,
    notify_success = TRUE) {
  # Ensure character vector, remove leading/trailing whitespace
  x_as_trimmed_char <- trimws(as.character(x))
  suppressWarnings({
    out <- as.numeric(x_as_trimmed_char)
    failed <- !is.na(x_as_trimmed_char) & is.na(out)
    if (any(failed)) {
      if (notify_failure == TRUE) {
        failed_indices <- which(failed)
        failed_values <- x_as_trimmed_char[failed_indices]
        n_to_show <- min(10, length(failed_values))
        message(
          "The following ", length(failed_indices), " entr",
          ifelse(length(failed_indices) == 1, "y", "ies"),
          " could not be converted to numeric.\n",
          paste0(
            "Index/Value pairs: ",
            paste0(
              failed_indices[1:n_to_show], " = ",
              failed_values[1:n_to_show], collapse = " | ")),
          "\nReturning the original vector.")
      }
      return(x)
    } else {
      if (notify_success == TRUE) {

        message("The vector was successfully converted to numeric.")
      }
      return(out)
    }
  })
}
