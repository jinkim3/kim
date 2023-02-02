#' Standardize
#'
#' Standardize (i.e., normalize, obtain z-scores, or obtain the
#' standard scores)
#'
#' @param x a numeric vector
#' @return the output will be a vector of the standard scores of the input.
#' @examples
#' standardize(1:10)
#' @export
standardize <- function(x = NULL) {
  return(kim::z_score(x))
}
