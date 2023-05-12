#' Tau-squared (between-studies variance for meta analysis)
#'
#' Calculate tau-squared, the between-studies variance (the variance of the
#' effect size parameters across the population of studies), as illustrated
#' in Borenstein et al. (2009, pp. 72-73, ISBN: 978-0-470-05724-7).
#'
#' Negative values of tau-squared are converted to 0 in the output (see
#' Cheung, 2013;
#' https://web.archive.org/web/20230512225539/https://openmx.ssri.psu.edu/thread/2432)
#'
#' @param effect_sizes effect sizes (e.g., standardized mean differences)
#' @param effect_size_variances within-study variances
#' @examples
#' \dontrun{
#' tau_squared(effect_sizes = c(1, 2), effect_size_variances = c(3, 4))
#' # a negative tau squared value is converted to 0:
#' tau_squared(effect_sizes = c(1.1, 1.4), effect_size_variances = c(1, 4))
#' }
#' @export
tau_squared <- function(
    effect_sizes = NULL,
    effect_size_variances = NULL) {
  # check inputs ----
  if (is.null(effect_sizes)) {
    stop("Please provide an input for the 'effect_sizes' argument.")
  }
  if (is.null(effect_size_variances)) {
    stop("Please provide an input for the 'effect_size_variances' argument.")
  }
  if (length(effect_sizes) != length(effect_size_variances)) {
    stop("The effect sizes and their variances are of different lengths.")
  } else {
    # k stands for number of studies
    k <- length(effect_sizes)
  }
  # weights ----
  weights <- 1 / effect_size_variances
  # the q statistic ----
  q <- sum(weights * effect_sizes ^ 2) -
    ((sum(weights * effect_sizes)) ^ 2 / sum(weights))
  # df ----
  df <- k - 1
  # the c statistic ----
  c <- sum(weights) - sum(weights ^ 2) / sum(weights)
  # tau squared ----
  tau_squared <- (q - df) / c
  # if tau squared is negative, set it to 0
  if (tau_squared < 0) {
    tau_squared <- 0
  }
  return(tau_squared)
}
