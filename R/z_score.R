#' z score
#'
#' Calculate z-scores (i.e., standardize or obtain the standard scores)
#'
#' @param x a numeric vector
#' @param na.rm logical. If \code{na.rm = TRUE}, NA values in the vector
#' will be removed before calculating z-scores (default = TRUE).
#' @return the output will be a vector of z-scores.
#' @examples
#' z_score(1:10)
#' @export
z_score <- function(x = NULL, na.rm = TRUE) {
  return((x - mean(x, na.rm = na.rm)) / stats::sd(x, na.rm = na.rm))
}
