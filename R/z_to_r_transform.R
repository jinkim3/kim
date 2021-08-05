#' Z to r transformation (Inverse of Fisher's Z transformation)
#'
#' Perform the Z-to-r transformation (i.e., the inverse of Fisher's
#' r-to-Z transformation) for given Z value(s).
#'
#' @param z a (vector of) Z values
#' @return the output will be a vector of correlation coefficient(s) that
#' are the result(s) of the Z-to-r transformation.
#' @examples
#' z_to_r_transform(2.646652)
#' z_to_r_transform(z = -3:3)
#' @export
z_to_r_transform <- function(z = NULL) {
  return((exp(2 * z) - 1) / (exp(2 * z) + 1))
}
