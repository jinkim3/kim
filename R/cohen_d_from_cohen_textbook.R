#' Cohen's d from Jacob Cohen's textbook (1988)
#'
#' Calculates Cohen's d as described in Jacob Cohen's textbook (1988),
#' Statistical Power Analysis for the Behavioral Sciences, 2nd Edition
#'
#' @param sample_1_as_vector a vector of values in the first of two samples
#' @param sample_2_as_vector a vector of values in the second of two samples
#' @examples
#' cohen_d_from_cohen_textbook(1:10, 3:12)
#' @export
cohen_d_from_cohen_textbook <- function(
  sample_1_as_vector, sample_2_as_vector) {
  s1 <- sample_1_as_vector[!is.na(sample_1_as_vector)]
  s2 <- sample_2_as_vector[!is.na(sample_2_as_vector)]
  output <- (mean(s1) - mean(s2)) / sqrt(
    (sum((s1 - mean(s1))^2) + sum((s2 - mean(s2))^2)) /
      (length(s1) + length(s2) - 2))
  return(output)
}
