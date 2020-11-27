#' Cohen's d from Jacob Cohen's textbook (1988)
#'
#' Calculates Cohen's d as described in Jacob Cohen's textbook (1988),
#' Statistical Power Analysis for the Behavioral Sciences, 2nd Edition
#'
#' @param sample_1 a vector of values in the first of two samples
#' @param sample_2 a vector of values in the second of two samples
#' @param data a data object (a data frame or a data.table)
#' @param iv_name name of the independent variable
#' @param dv_name name of the dependent variable
#' @examples
#' cohen_d_from_cohen_textbook(1:10, 3:12)
#' cohen_d_from_cohen_textbook(
#' data = mtcars, iv_name = "vs", dv_name = "mpg")
#' @export
cohen_d_from_cohen_textbook <- function(
  sample_1 = NULL, sample_2 = NULL,
  data = NULL, iv_name = NULL, dv_name = NULL) {
  if (!is.null(sample_1) & !is.null(sample_2)) {
    s1 <- sample_1[!is.na(sample_1)]
    s2 <- sample_2[!is.na(sample_2)]
  }
  # if data object is provided
  if (!is.null(data) & !is.null(iv_name) & !is.null(dv_name)) {
    if (length(unique(data[[iv_name]])) != 2) {
      stop(paste0(
        "The independent variable has ",
        length(unique(data[[iv_name]])), " levels.\n",
        "Cohen's d can be calculated when there are exactly 2 levels."))
    } else {
      s1 <- subset(
        data, get(iv_name) == sort(unique(data[[iv_name]]))[1])
      s2 <- subset(
        data, get(iv_name) == sort(unique(data[[iv_name]]))[2])
      s1 <- s1[!is.na(s2)]
      s2 <- s2[!is.na(s2)]
    }
  }
  output <- (mean(s1) - mean(s2)) / sqrt(
    (sum((s1 - mean(s1))^2) + sum((s2 - mean(s2))^2)) /
      (length(s1) + length(s2) - 2))
  return(output)
}
