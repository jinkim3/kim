#' z score
#'
#' Calculate z-scores (i.e., standardize or obtain the standard scores)
#'
#' @param x a numeric vector
#' @return the output will be a vector of z-scores.
#' @examples
#' z_score(1:10)
#' @export
z_score <- function(x = NULL) {
  return(x - mean(x) / stats::sd(x))
}
