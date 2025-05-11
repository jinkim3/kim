#' Convert a vector to numeric if appropriate
#'
#' Attempt to convert a vector to numeric. Handles trimming, coercion,
#' and optionally notifies of conversion success/failure.
#'
#' @param x A vector to attempt to convert.
#' @param verbose Logical. Should messages about conversion be shown?
#' Default = TRUE.
#' @param strict Logical. If TRUE, return original vector if any values fail.
#' If FALSE, returns numeric vector with NAs where conversion fails.
#' Default = TRUE.
#' @return A numeric vector, or the original input if strict=TRUE and conversion fails.
#' @examples
#' \donttest{
#' # Successful conversions from character
#' convert_to_numeric_if_ok(c("123", " 456 ", "-78.9", "+0.5", "1e3"))
#' convert_to_numeric_if_ok(c("Inf", "-Inf", "NaN"))  # special values
#'
#' # Input is already numeric
#' convert_to_numeric_if_ok(1:3)  # returns same vector
#'
#' # Logical input (TRUE = 1, FALSE = 0)
#' convert_to_numeric_if_ok(c(TRUE, FALSE))
#'
#' # Mixed types are coerced before entering function
#' convert_to_numeric_if_ok(c(1, "2"))  # actually character vector
#'
#' # Factor input (coerced to character first)
#' convert_to_numeric_if_ok(factor(c("1", "2", "bad")))  # includes one invalid
#'
#' # Partial failure with default strict = TRUE (returns original)
#' convert_to_numeric_if_ok(c("1", "a", "3"))
#'
#' # Partial failure with strict = FALSE (returns numeric with NA)
#' convert_to_numeric_if_ok(c("1", "a", "3"), strict = FALSE)
#'
#' # Suppress all messages
#' convert_to_numeric_if_ok(
#' c("1", "a", "3"), strict = FALSE, verbose = FALSE)
#'
#' # Edge cases: whitespace, empty strings, missing values
#' convert_to_numeric_if_ok(c("", " ", "\t\n", NA))
#' convert_to_numeric_if_ok(c("NA", NA))  # "NA" as string vs NA
#'
#' # All invalid inputs (strict = FALSE)
#' convert_to_numeric_if_ok(c("abc", "$100", "1.2.3"), strict = FALSE)
#'
#' # Mixed sanity test vector
#' crazy_vec <- c(
#'   "123", " 456 ", "-78.9", "+0.5", "1e-3",  # valid
#'   "1.2.3", "abc123", "$100", "NaN", "Inf", # mixed
#'   "-Inf", "TRUE", "", " ", "\t\n", NA      # edge + invalid
#' )
#' convert_to_numeric_if_ok(crazy_vec, strict = FALSE)
#' }
#'
#' @export
#' @import data.table
convert_to_numeric_if_ok <- function(
    x,
    verbose = TRUE,
    strict = TRUE
) {
  # Ensure character vector, trim whitespace
  x_as_trimmed_char <- trimws(as.character(x))
  # Attempt conversion and capture warnings (selectively)
  out <- suppressWarnings(as.numeric(x_as_trimmed_char))
  # Identify conversion failures
  failed <- !is.na(x_as_trimmed_char) & is.na(out)
  n_failed <- sum(failed)
  if (n_failed > 0) {
    if (verbose) {
      failed_indices <- which(failed)
      failed_values <- x_as_trimmed_char[failed_indices]
      n_to_show <- min(10, length(failed_values))
      message(
        "Conversion failed for ", n_failed, " entr",
        ifelse(n_failed == 1, "y", "ies"), ".\n",
        paste0(
          "Index/Value pairs: ",
          paste0(
            failed_indices[1:n_to_show], " = ",
            failed_values[1:n_to_show], collapse = " | ")))
    }
    if (strict) {
      if (verbose) message(
        "Returning original vector due to strict = TRUE.")
      return(x)
    } else {
      if (verbose) message(
        "Returning partially converted numeric vector with NA values.")
      return(out)
    }
  } else {
    if (verbose) message(
      "The vector was successfully converted to numeric.")
    return(out)
  }
}
