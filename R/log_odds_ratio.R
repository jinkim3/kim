#' Log odds ratio
#'
#' Calculate log odds ratio (i.e., ln of odds ratio), as illustrated
#' in Borenstein et al. (2009, p. 36, ISBN: 978-0-470-05724-7)
#'
#' @param data a data object (a data frame or a data.table)
#' @param iv_name name of the independent variable (grouping variable)
#' @param dv_name name of the dependent variable (binary outcome)
#' @param contingency_table a contingency table, which can be directly
#' entered as an input for calculating the odds ratio
#' @param ci size of the confidence interval. Input can be any value
#' less than 1 and greater than or equal to 0. By default, \code{ci = 0.95}.
#' If \code{ci = TRUE}, the default value of 0.95 will be used. If \code{
#' ci = FALSE}, no confidence interval will be estimated.
#' @param var_include logical. Should the output include
#' variance of the log of odds ratio? (default = FALSE)
#' @param invert logical. Whether the inverse of the odds ratio
#' (i.e., 1 / odds ratio) should be returned.
#' @examples
#' \dontrun{
#' log_odds_ratio(data = mtcars, iv_name = "vs", dv_name = "am")
#' log_odds_ratio(contingency_table = matrix(c(5, 10, 95, 90), nrow = 2))
#' log_odds_ratio(contingency_table = matrix(c(5, 10, 95, 90), nrow = 2),
#' invert = TRUE)
#' log_odds_ratio(contingency_table = matrix(c(34, 39, 16, 11), nrow = 2))
#' log_odds_ratio(contingency_table = matrix(c(34, 39, 16, 11), nrow = 2),
#' var_include = TRUE)
#' }
#' @export
log_odds_ratio <- function(
    data = NULL,
    iv_name = NULL,
    dv_name = NULL,
    contingency_table = NULL,
    ci = 0.95,
    var_include = FALSE,
    invert = FALSE) {
  # check if ci input is valid
  if (ci == TRUE) {
    ci <- 0.95
  }
  if (ci < 0 | ci >= 1) {
    stop("The input for 'ci' must be in the range [0, 1).")
  }
  # contingency table
  if (is.null(contingency_table)) {
    # convert to data.table
    dt <- data.table::setDT(data.table::copy(data))[
      , c(iv_name, dv_name), with = FALSE]
    # remove rows with na
    dt <- stats::na.omit(dt)
    # check if iv is binary
    num_of_levels_in_iv <- length(unique(dt[[iv_name]]))
    if (num_of_levels_in_iv != 2) {
      stop(paste0(
        "The independent variable has ", num_of_levels_in_iv,
        " levels.\n",
        "The current version of the function can only handle",
        " an independent variable with exactly two levels."))
    }
    # check if dv is binary
    num_of_levels_in_dv <- length(unique(dt[[dv_name]]))
    if (num_of_levels_in_dv != 2) {
      stop(paste0(
        "The dependent variable has ", num_of_levels_in_dv,
        " levels.\n",
        "The current version of the function can only handle",
        " a dependent variable with exactly two levels."))
    }
    contingency_table <- table(
      dt[[iv_name]], dt[[dv_name]])
  }
  # contingency_table <- matrix(c(5, 10, 95, 90), nrow = 2)
  odds_ratio <- (contingency_table[1, 1] * contingency_table[2, 2]) /
    (contingency_table[2, 1] * contingency_table[1, 2])
  if (invert == TRUE) {
    odds_ratio <- 1 / odds_ratio
  }
  log_odds_ratio <- log(odds_ratio)
  # approximate variance
  var_of_log_odds_ratio <-
    1 / contingency_table[1, 1] +
    1 / contingency_table[1, 2] +
    1 / contingency_table[2, 1] +
    1 / contingency_table[2, 2]
  se_of_log_odds_ratio <- sqrt(var_of_log_odds_ratio)
  if (ci == FALSE) {
    output <- c(ln_odds_ratio = log_odds_ratio)
  } else {
    # critical values for estimating the confidence interval
    cv <- c((1 - ci) / 2, 1 - (1 - ci) / 2)
    odds_ratio_ci <- exp(
      log_odds_ratio + se_of_log_odds_ratio * stats::qnorm(cv))
    # ll stands for lower limit; ul for upper limit
    odds_ratio_ll <- min(odds_ratio_ci)
    odds_ratio_ul <- max(odds_ratio_ci)
    output <- c(
      log_odds_ratio = log_odds_ratio,
      log_odds_ratio_ll = log(odds_ratio_ll),
      log_odds_ratio_ul = log(odds_ratio_ul))
  }
  if (var_include == TRUE) {
    output <- c(output, var_of_log_odds_ratio = var_of_log_odds_ratio)
  }
  return(output)
}
