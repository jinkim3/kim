#' Convert Cohen's d to r
#'
#' Convert d (standardized mean difference or Cohen's d) to r (correlation),
#' as illustrated in Borenstein et al. (2009, p. 48, ISBN: 978-0-470-05724-7)
#'
#' @param d Cohen's d (the input can be a vector of values)
#' @param n1 sample size in the first of two group (the input can be a
#' vector of values)
#' @param n2 sample size in the second of two group (the input can be a
#' vector of values)
#' @param d_var (optional argument) variance of d
#' (the input can be a vector of values). If this argument receives an
#' input, variance of r will be returned as well.
#' @return the output will be a vector of correlation values
#' (and variances of r if the argument d_var received an input)
#' @examples
#' \dontrun{
#' cohen_d_to_r(1)
#' cohen_d_to_r(d = 1:3)
#' cohen_d_to_r(d = 1:3, n1 = c(100, 200, 300), n2 = c(50, 250, 900))
#' cohen_d_to_r(1.1547)
#' cohen_d_to_r(d = 1.1547, d_var = .0550)
#' cohen_d_to_r(d = 1:2, d_var = 1:2)
#' }
#' @export
cohen_d_to_r <- function(
    d = NULL,
    n1 = NULL,
    n2 = NULL,
    d_var = NULL) {
  # check arguments
  if (!is.null(n1) & !is.null(n2)) {
    a <- (n1 + n2) ^ 2 / (n1 * n2)
  } else if (is.null(n1) & is.null(n2)) {
    # if n1 and n2 are unknown, a = 4 by the formula
    a <- 4
  } else {
    stop("Please provide values of both n1 and n2, or provide neither.")
  }
  r <- d / (sqrt(d ^ 2 + a))
  if (!is.null(d_var)) {
    r_var <- (a ^ 2 * d_var) / (d ^ 2 + a) ^ 3
    output <- c(r = r, r_var = r_var)
    return(output)
  }
  return(r)
}
