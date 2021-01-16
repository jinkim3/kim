#' Top, median, or bottom
#'
#' Indicates whether each value in a vector belongs to top, median, or bottom
#'
#' @param vector a numeric vector
#' @return a character vector indicating whether each element
#' in a vector belongs to "top", "median", or "bottom"
#' @examples
#' top_median_or_bottom(c(1, 2, 3, NA))
#' top_median_or_bottom(c(1, 2, 2, NA))
#' top_median_or_bottom(c(1, 1, 2, NA))
#' @export
top_median_or_bottom <- function(vector) {
  v_no_na <- vector[!is.na(vector)]
  median <- stats::median(v_no_na)
  output <- data.table::fcase(
    vector == median, "median",
    vector > median, "top",
    vector < median, "bottom"
  )
  return(output)
}
