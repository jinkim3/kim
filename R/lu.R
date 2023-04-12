#' lu: Length of unique values
#'
#' Extract unique elements and get the length of those elements
#'
#' @param x a vector or a data frame or an array or NULL.
#' @return a vector, data frame, or array-like 'x' but with duplicate
#' elements/rows removed.
#' @examples
#' unique(c(10, 3, 7, 10))
#' lu(c(10, 3, 7, 10))
#' unique(c(10, 3, 7, 10, NA))
#' lu(c(10, 3, 7, 10, NA))
#' lu(c("b", "z", "b", "a", NA, NA, NA))
#' @export
lu <- function(x = NULL) {
  return(length(unique(x)))
}
