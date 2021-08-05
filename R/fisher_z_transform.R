#' Fisher's Z transformation
#'
#' Perform Fisher's r-to-Z transformation for given correlation coefficient(s).
#'
#' @param r a (vector of) correlation coefficient(s)
#' @return the output will be a vector of Z values which were transformed
#' from the given r values.
#' @examples
#' fisher_z_transform(0.99)
#' fisher_z_transform(r = seq(0.1, 0.5, 0.1))
#' @export
fisher_z_transform <- function(r = NULL) {
  return(1/2 * log((1 + r) / (1 - r)))
}
