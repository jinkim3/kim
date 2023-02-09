#' Convert log odds ratio to Cohen's d
#'
#' Convert log odds ratio to Cohen'd (standardized mean difference),
#' as illustrated in Borenstein et al. (2009, p. 47, ISBN: 978-0-470-05724-7)
#'
#' @param log_odds_ratio log odds ratio (the input can be a vector of values),
#' which will be converted to Cohen's d
#' @examples
#' \dontrun{
#' log_odds_ratio_to_d(log(1))
#' log_odds_ratio_to_d(log(2))
#' }
#' @export
log_odds_ratio_to_d <- function(
    log_odds_ratio = NULL) {
  d <- log_odds_ratio * sqrt(3) / pi
  return(d)
}
