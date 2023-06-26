#' Mean center
#'
#' Mean-center a variable, i.e., subtract the mean of a numeric vector
#' from each value in the numeric vector
#'
#' @param x a numeric vector; though not thoroughly tested, the function
#' can accept a matrix as an input.
#' @examples
#' mean_center(1:5)
#' mean_center(1:6)
#' # if the input is a matrix
#' matrix(1:9, nrow = 3)
#' mean_center(matrix(1:9, nrow = 3))
#' @export
mean_center <- function(x) {
  output <- scale(x = x, center = TRUE, scale = FALSE)
  # if the output is a vector, return the vector
  if (dim(output)[2] == 1) {
    output <- as.vector(output)
  }
  return(output)
}
