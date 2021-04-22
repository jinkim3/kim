#' su: Sorted unique values
#'
#' Extract unique elements and sort them
#'
#' @param x a vector or a data frame or an array or NULL.
#' @return a vector, data frame, or array-like 'x' but with duplicate
#' elements/rows removed.
#' @examples
#' su(c(10, 3, 7, 10))
#' su(c("b", "z", "b", "a"))
#' @export
su <- function(x = NULL) {
  return(sort(unique(x)))
}
