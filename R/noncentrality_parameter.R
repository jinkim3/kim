#' Find noncentrality parameter
#'
#' Find noncentrality parameter
#'
#' @param t_stat the t-statistic associated with the noncentrality parameters
#' @param df degrees of freedom associated with the noncentrality parameters
#' @param initial_value initial value of the noncentrality parameter for
#' optimization (default = 0). Adjust this value if results look strange.
#' @param ci width of the confidence interval associated with the
#' noncentrality parameters (default = 0.95)
#' @examples
#' \donttest{
#' noncentrality_parameter(4.29, 9)
#' }
#' @export
noncentrality_parameter <- function(
  t_stat, df, initial_value = 0, ci = 0.95) {
  # desired area under the noncentral t distribution curve
  desired_p <- c(1 - (1 - ci) / 2, (1 - ci) / 2)
  # function for which to find the minimum
  ncp_fn <- function(ncp_value, ..t_stat, ..df, ..desired_p) {
    output <- abs(stats::pt(
      q = ..t_stat, df = ..df, ncp = ncp_value) - ..desired_p)
    return(output)
  }
  # optim
  ncp_values <- vapply(seq_along(desired_p), function(i) {
    optim_results <- suppressWarnings(
      stats::optim(par = initial_value, fn = ncp_fn, ..t_stat = t_stat,
            ..df = df, ..desired_p = desired_p[i]))
    if (optim_results$convergence == 0) {
      return(optim_results$par)
    } else {
      return(NA_real_)
    }
  }, FUN.VALUE = numeric(1L))
  # output
  return(ncp_values)
}
