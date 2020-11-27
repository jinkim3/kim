#' Summarize multiple regression results in a data.table
#'
#' @param data a data object (a data frame or a data.table)
#' @param formula a formula object for the regression equation
#' @param sigfigs number of significant digits to round to
#' @param round_digits_after_decimal round to nth digit after decimal
#' (alternative to \code{sigfigs})
#' @examples
#' multiple_regression(data = mtcars, formula = mpg ~ gear * cyl)
#' @export
multiple_regression <- function(
  data = NULL,
  formula = NULL,
  sigfigs = NULL,
  round_digits_after_decimal = NULL) {
  # regression model
  model <- stats::lm(formula = formula, data = data)
  model_summary <- summary(model)
  # get the results
  reg_results <- model_summary[["coefficients"]]
  variable <- row.names(reg_results)
  estimate <- reg_results[, "Estimate"]
  se <- reg_results[, "Std. Error"]
  t_stat <- reg_results[, "t value"]
  p_value <- pretty_round_p_value(reg_results[, "Pr(>|t|)"])
  r_squared <- model_summary[["r.squared"]]
  adj_r_squared <- model_summary[["adj.r.squared"]]
  df_model <- model_summary[["fstatistic"]][["numdf"]]
  df_residual <- model_summary[["fstatistic"]][["dendf"]]
  f_stat <- model_summary[["fstatistic"]][["value"]]
  model_p_value <- pretty_round_p_value(
    stats::pf(f_stat, df_model, df_residual, lower.tail = FALSE),
    include_p_equals = TRUE
  )
  n <- nrow(stats::model.frame(model))
  # standardized betas
  std_beta <- lm.beta::lm.beta(model)[["standardized.coefficients"]]
  # rounding
  if (!is.null(sigfigs) & !is.null(round_digits_after_decimal)) {
    stop(paste0(
      "Round to nth digit or n sigfigs? ",
      "You can provide a value for EITHER argument, but NOT both."
    ))
  }
  if (is.null(sigfigs) & is.null(round_digits_after_decimal)) {
    sigfigs <- 3
  }
  if (!is.null(sigfigs)) {
    estimate <- signif(estimate, sigfigs)
    se <- signif(se, sigfigs)
    std_beta <- signif(std_beta, sigfigs)
    t_stat <- signif(t_stat, sigfigs)
    r_squared <- signif(r_squared, sigfigs)
    adj_r_squared <- signif(adj_r_squared, sigfigs)
    f_stat <- signif(f_stat, sigfigs)
  }
  if (!is.null(round_digits_after_decimal)) {
    estimate <- round(estimate, round_digits_after_decimal)
    se <- round(se, round_digits_after_decimal)
    std_beta <- round(std_beta, round_digits_after_decimal)
    t_stat <- round(t_stat, round_digits_after_decimal)
    r_squared <- round(r_squared, round_digits_after_decimal)
    adj_r_squared <- round(
      adj_r_squared, round_digits_after_decimal
    )
    f_stat <- round(f_stat, round_digits_after_decimal)
  }
  t1 <- data.table::data.table(
    variable, estimate, se, std_beta, t_stat, p_value
  )
  # add an empty row
  t2 <- rbind(t1, as.list(rep("", ncol(t1))))
  # add model stats
  variable_2 <- c(
    paste0("R-squared: ", r_squared),
    paste0("Adj. R-squared: ", adj_r_squared),
    paste0("F(", df_model, ", ", df_residual, ") = ", f_stat),
    paste0("Model ", model_p_value),
    paste0("N = ", n),
    paste0("DV: ", all.vars(formula[[2]]))
  )
  t3 <- data.table::data.table(
    variable = variable_2,
    estimate = rep("", length(variable_2)),
    se = rep("", length(variable_2)),
    std_beta = rep("", length(variable_2)),
    t_stat = rep("", length(variable_2)),
    p_value = rep("", length(variable_2))
  )
  t4 <- rbind(t2, t3)
  return(t4)
}
