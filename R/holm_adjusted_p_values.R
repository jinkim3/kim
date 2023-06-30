#' Holm-adjusted p-values
#'
#' Adjust a vector of p-values using the method proposed by Holm
#'
#' See the following reference:
#' Holm 1979 <https://www.jstor.org/stable/4615733>
#' Manual for the 'p.adjust' function in the 'stats' package
#' <https://stat.ethz.ch/R-manual/R-devel/library/stats/html/p.adjust.html>
#'
#' @param p a numeric vector of p-values
#' @examples
#' holm_adjusted_p(c(.05, .01))
#' holm_adjusted_p(c(.05, .05, .05))
#' @export
#' @import data.table
holm_adjusted_p <- function(p = NULL) {
  return(stats::p.adjust(p = p, method = "holm"))
}
