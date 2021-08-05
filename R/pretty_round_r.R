#' Pretty round r
#'
#' Round correlation coefficients in APA style (7th Ed.)
#'
#' @param r a (vector of) correlation coefficient(s)
#' @param round_digits_after_decimal how many digits after the decimal
#' point should the p-value be rounded to? (default = 2)
#' @return the output will be a character vector of correlation coefficient(s).
#' @examples
#' pretty_round_r(r = -0.123)
#' pretty_round_r(c(-0.12345, 0.45678), round_digits_after_decimal = 3)
#' pretty_round_r(c(-0.12, 0.45), round_digits_after_decimal = 4)
#' @export
pretty_round_r <- function(
  r = NULL,
  round_digits_after_decimal = 2) {
  return(sub("\\+?0", "", sprintf(
    paste0("%.", round_digits_after_decimal, "f"), r)))
}
