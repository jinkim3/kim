#' Find the overlapping interval of two ranges.
#'
#' This function should be applied to cases where the two ranges are
#' inclusive of both endpoints. For example, the function can work for
#' a pair of ranges like \[0, 1\] and \[3, 4\] but not for pairs like
#' \[0, 1\) and \(3, 5\)
#'
#' @param interval_1_begin a number at which the first interval begins
#' (the left INCLUSIVE endpoint of interval 1)
#' @param interval_1_end a number at which the first interval ends
#' (the right INCLUSIVE endpoint of interval 1)
#' @param interval_2_begin a number at which the second interval begins
#' (the left INCLUSIVE endpoint of interval 2)
#' @param interval_2_end a number at which the second interval ends
#' (the right INCLUSIVE endpoint of interval 2)
#' @return the output will be \code{NULL} if there is no overlapping
#' region or a vector of the endpoints of the overlapping interval.
#' @examples
#' overlapping_interval(1, 3, 2, 4)
#' overlapping_interval(1, 2.22, 2.22, 3)
#' @export
overlapping_interval <- function(
    interval_1_begin = NULL,
    interval_1_end = NULL,
    interval_2_begin = NULL,
    interval_2_end = NULL) {
  # check numeric inputs
  input_check_1 <- check_modes(
    interval_1_begin, interval_1_end,
    interval_2_begin, interval_2_end,
    mode_to_confirm = "numeric")
  if (input_check_1$logical == FALSE) {
    print(input_check_1$dt)
    stop(paste0("As shown above, ", tolower(input_check_1$summary)))
  }
  # check whether inputs are ordered
  if (!interval_1_begin <= interval_1_end) {
    stop("Please make sure that interval_1_begin <= interval_1_end")
  }
  if (!interval_2_begin <= interval_2_end) {
    stop("Please make sure that interval_2_begin <= interval_2_end")
  }
  # check whether the intervals overlap
  if (max(interval_1_begin, interval_2_begin) >
      min(interval_1_end, interval_2_end)) {
    return(NULL)
  }
  overlapping_interval <- sort(c(
    min(interval_1_end, interval_2_end),
    max(interval_1_begin, interval_2_begin)))
  return(overlapping_interval)
}
