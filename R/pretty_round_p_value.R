#' Pretty round p-value
#'
#' @param p_value_vector one number or a numeric vector
#' @param include_p_equals if \code{TRUE}, output will be a string of
#' mathematical expression including "p", e.g., "p < .01" (default = FALSE)
#' @param round_digits_after_decimal how many digits after the decimal
#' point should the p-value be rounded to?
#' @return the output will be a character vector with p values, e.g.,
#' a vector of strings like "< .001" (or "p < .001").
#' @examples
#' pretty_round_p_value(
#'   p_value_vector = 0.049,
#'   round_digits_after_decimal = 2, include_p_equals = FALSE
#' )
#' pretty_round_p_value(c(0.0015, 0.0014), include_p_equals = TRUE)
#' @export
#' @importFrom stats model.frame pf
pretty_round_p_value <- function(
  p_value_vector = NULL,
  round_digits_after_decimal = 3,
  include_p_equals = FALSE) {
  if (is.numeric(p_value_vector)) {
    if (include_p_equals == TRUE) {
      output <- ifelse(
        p_value_vector < .001, "p < .001",
        paste0(
          "p = ", sub(
            "^(-?)0.", "\\1.",
            sprintf(
              paste0("%.", round_digits_after_decimal, "f"),
              p_value_vector
            )
          )
        )
      )
      return(output)
    }
    output <- ifelse(
      p_value_vector < .001, "< .001",
      sub(
        "^(-?)0.", "\\1.",
        sprintf(
          paste0("%.", round_digits_after_decimal, "f"),
          p_value_vector
        )
      )
    )
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
