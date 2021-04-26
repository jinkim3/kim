#' su: Sorted unique values
#'
#' Extract unique elements and sort them
#'
#' @param x a vector or a data frame or an array or NULL.
#' @param na.last an argument to be passed onto the 'sort' function
#' (in base R) for controlling the treatment of NAs.
#' If TRUE, missing values in the data are put last; if FALSE, they
#' are put first; if NA, they are removed. By default, \code{na.last = TRUE}
#' @param decreasing logical. Should the sort be increasing or decreasing?
#' An argument to be passed onto the 'sort' function (in base R).
#' By default, \code{decreasing = FALSE}
#' @return a vector, data frame, or array-like 'x' but with duplicate
#' elements/rows removed.
#' @examples
#' su(c(10, 3, 7, 10, NA))
#' su(c("b", "z", "b", "a", NA, NA, NA))
#' @export
su <- function(
  x = NULL,
  na.last = TRUE,
  decreasing = FALSE) {
  return(sort(unique(x), na.last = na.last, decreasing = decreasing))
}
