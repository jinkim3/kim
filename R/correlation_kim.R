#' Estimate the correlation between two variables
#'
#' @param x a numeric vector of data values
#' @param y a numeric vector of data values
#' @param data (optional) a data object (a data frame or a data.table)
#' @param x_var_name (optional) name of the first variable
#' (if using a data set as an input)
#' @param y_var_name (optional) name of the second variable
#' (if using a data set as an input)
#' @param ci_range range of the confidence interval for the correlation
#' coefficient. If \code{ci_range = FALSE}, no confidence interval
#' will be estimated. By default, \code{ci_range = 0.95}.
#' @param round_r number of decimal places to which to round
#' correlation coefficients (default = 2)
#' @param round_p number of decimal places to which to round
#' p-values (default = 3)
#' @param output_type type of the output. If \code{output_type = "dt"},
#' the function's output will be a data.table with the results from the
#' correlation analysis. If \code{output_type = "summary"}, the function's
#' output will be a statement (a string) summarizing the results from
#' the correlation analysis. By default, \code{output_type = "summary"}
#' @examples
#' \dontrun{
#' correlation_kim(x = 1:4, y = c(1, 3, 2, 4))
#' correlation_kim(x = 1:4, y = c(1, 3, 2, 4), ci_range = FALSE)
#' # output as a data table
#' correlation_kim(x = 1:4, y = c(1, 3, 2, 4), output_type = "dt")
#' }
#' @export
correlation_kim <- function(
  x = NULL,
  y = NULL,
  data = NULL,
  x_var_name = NULL,
  y_var_name = NULL,
  ci_range = 0.95,
  round_r = 2,
  round_p = 3,
  output_type = "summary") {
  # check inputs
  if (!is.null(data) & !is.null(x_var_name) & !is.null(y_var_name)) {
    x <- data[[x_var_name]]
    y <- data[[y_var_name]]
  }
  corr_results <- stats::cor.test(
    x = x,
    y = y,
    conf.level = ci_range)
  r <- corr_results[["estimate"]]
  df <- corr_results[["parameter"]][["df"]]
  r_ci_ll <- min(corr_results[["conf.int"]])
  r_ci_ul <- max(corr_results[["conf.int"]])
  p <- corr_results[["p.value"]]
  if (output_type == "dt") {
    results_dt <- data.table::data.table(
      r,
      df,
      r_ci_ll,
      r_ci_ul,
      p
    )
    return(results_dt)
  }
  results_pt_1 <- paste0(
    "r(", df, ") = ",
    kim::pretty_round_r(r = r, round_digits_after_decimal = round_r),
    ", ")
  results_pt_2 <- ifelse(
    ci_range == FALSE, "", paste0(
      ci_range * 100,
      "% CI = [",
      kim::pretty_round_r(r = r_ci_ll, round_digits_after_decimal = round_r),
      ", ",
      kim::pretty_round_r(r = r_ci_ul, round_digits_after_decimal = round_r),
      "], "
    ))
  results_pt_3 <-
    kim::pretty_round_p_value(
      p, round_digits_after_decimal = round_p,
      include_p_equals = TRUE)
  output <- paste0(
    results_pt_1, results_pt_2, results_pt_3)
  return(output)
}
