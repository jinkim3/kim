#' Coefficient of variation
#'
#' Calculates the (population or sample) coefficient of variation
#' of a given numeric vector
#'
#' @param vector a numeric vector
#' @param pop_or_sample should coefficient of variation be calculated for
#' a "population" or a "sample"?
#' @return a numeric value
#' @examples
#' coefficent_of_variation(1:4, pop_or_sample = "sample")
#' coefficent_of_variation(1:4, pop_or_sample = "pop")
#' @export
coefficent_of_variation <- function(
  vector, pop_or_sample = "pop") {
  # deal with NA values
  v_no_na <- vector[!is.na(vector)]
  # population or sample?
  if (pop_or_sample == "pop") {
    pop_var <- mean((v_no_na - mean(v_no_na))^2)
    output <- sqrt(pop_var) / mean(v_no_na)
  } else if (pop_or_sample == "sample") {
    output <- stats::sd(v_no_na) / mean(v_no_na)
  }
  return(output)
}
