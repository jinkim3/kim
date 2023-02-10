#' Convert variance of log odds ratio to variance of d
#'
#' Convert the variance of a log odds ratio to the variance of
#' a Cohen'd (standardized mean difference), as illustrated in
#' Borenstein et al. (2009, p. 47, ISBN: 978-0-470-05724-7)
#'
#' @param var_of_log_odds_ratio the variance of a log odds ratio
#' (the input can be a vector of values)
#' @examples
#' \dontrun{
#' var_of_log_odds_ratio_to_var_of_d(1)
#' }
#' @export
var_of_log_odds_ratio_to_var_of_d <- function(
    var_of_log_odds_ratio = NULL) {
  var_of_d <- var_of_log_odds_ratio * 3 / (pi ^ 2)
  return(var_of_d)
}
