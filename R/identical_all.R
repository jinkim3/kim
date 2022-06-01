#' Check whether all inputs are identical
#'
#' @param ... two or more R objects. If a vector or list is entered as
#' an input, the function will test whether the vector's or list's
#' elements are identical.
#' @return the output will be \code{TRUE} if all inputs are identical
#' or \code{FALSE} if not
#' @examples
#' identical_all(1:3, 1:3) # should return TRUE
#' identical_all(1:3, 1:3, 1:3, 1:3, 1:3) # should return TRUE
#' identical_all(1:3, 1:3, 1:3, 1:3, 1:3, 1:4) # should return FALSE
#' identical_all(1:10) # should return FALSE
#' identical_all(rep(1, 100)) # should return TRUE
#' identical_all(list(1, 1, 1)) # should return TRUE
#' identical_all(TRUE, FALSE) # should return FALSE
#' identical_all(FALSE, TRUE) # should return FALSE
#' @export
identical_all <- function(...) {
  # list of objects ----
  input_1 <- as.list(match.call(expand.dots = FALSE))[["..."]]
  # return(input_1)
  # if a vector or list is entered as an input ----
  if (length(input_1) < 2) {
    input_2 <- unlist(list(...), recursive = FALSE)
    if (is.list(input_2) == FALSE) {
      if (is.vector(input_2) == FALSE) {
        stop(paste0(
          "The input must be multiple objects separated by comma, ",
          "e.g., identical_all(1:3, 1:4), or the input must be ",
          "a vector or a list containing multiple objects."))
      }
    }
    input_1 <- input_2
  }
  # sequentially check whether each pair of elements are identical ----
  if (is.list(input_1) == TRUE) {
    results_from_identical_fn <- vapply(
      utils::head(seq_along(input_1), -1), function(i) {
        return(identical(input_1[[i]], input_1[[i + 1]]))
      }, logical(1L))
  } else if (is.vector(input_1) == TRUE) {
    results_from_identical_fn <- vapply(
      utils::head(seq_along(input_1), -1), function(i) {
        return(identical(input_1[i], input_1[i + 1]))
      }, logical(1L))
  } else {
    stop(paste0(
      "The function failed to transform the input(s) ",
      "into a list or a vector."))
  }
  # final output based on the results from above ----
  if (all(results_from_identical_fn)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
