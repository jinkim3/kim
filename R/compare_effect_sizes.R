#' Compare effect sizes
#'
#' Compares effect sizes
#' See p. 156 of Borenstein et al. (2009, ISBN: 978-0-470-05724-7).
#'
#' @param effect_sizes a vector of estimated effect sizes
#' @param effect_size_variances a vector of variances of the effect sizes
#' @param round logical. Should the statistics be rounded? (default = TRUE)
#' @param round_p number of decimal places to which to round
#' p-values (default = 3)
#' @param round_se number of decimal places to which to round the
#' standard errors of the difference (default = 2)
#' @param round_z number of decimal places to which to round the
#' z-statistic (default = 2)
#' @param pretty_round_p_value logical. Should the p-values be rounded
#' in a pretty format (i.e., lower threshold: "<.001").
#' By default, \code{pretty_round_p_value = TRUE}.
#'
#' @examples
#' \donttest{
#' compare_effect_sizes(
#' effect_sizes = c(0.6111, 0.3241, 0.5),
#' effect_size_variances = c(.0029, 0.0033, 0.01))
#' }
#' @export
#' @import data.table
compare_effect_sizes <- function(
  effect_sizes = NULL,
  effect_size_variances = NULL,
  round = TRUE,
  round_p = 3,
  round_se = 2,
  round_z = 2,
  pretty_round_p_value = TRUE) {
  # bind the vars locally to the function
  diff <- NULL
  # set up pairs
  es_pairs <- t(utils::combn(effect_sizes, 2))
  esv_pairs <- t(utils::combn(effect_size_variances, 2))
  dt <- data.table::data.table(es_pairs, esv_pairs)
  names(dt) <- c(paste0("es_", 1:2), paste0("es_", 1:2, "_var"))
  # differences
  dt[, diff := es_1 - es_2]
  # se of differences
  dt[, se_diff := sqrt(es_1_var + es_2_var)]
  # z of differences
  dt[, z_diff := diff / se_diff]
  # p values
  dt[, one_tailed_p := stats::pnorm(
    q = abs(z_diff), lower.tail = FALSE)]
  dt[, two_tailed_p := one_tailed_p * 2]
  # round
  if (round == TRUE) {
    dt[, se_diff := round(se_diff, round_se)]
    dt[, z_diff := round(z_diff, round_z)]
    # pretty round p
    if (pretty_round_p_value == TRUE) {
      dt[, one_tailed_p := kim::pretty_round_p_value(
        one_tailed_p, round_p)]
      dt[, two_tailed_p := kim::pretty_round_p_value(
        two_tailed_p, round_p)]
    } else {
      dt[, one_tailed_p := round(one_tailed_p, round_p)]
      dt[, two_tailed_p := round(two_tailed_p, round_p)]
    }
  }
  # output
  return(dt)
}
