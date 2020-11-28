#' Population variance of a vector
#'
#' Calculates the population variance, rather than the sample variance,
#' of a vector
#'
#' @param vector a numeric vector
#' @param na.rm if \code{TRUE}, NA values will be removed before calculation
#' @examples
#' population_variance(1:4)
#' var(1:4)
#' @export
population_variance <- function(vector, na.rm = TRUE) {
  if (na.rm == TRUE) {
    v2 <- vector[!is.na(vector)]
  } else {
    v2 <- vector
  }
  n <- length(v2)
  output <- stats::var(v2, na.rm = na.rm) * (n - 1) / n
  return(output)
}
