#' Multiple regression
#'
#' Conduct multiple regression analysis and summarize the results
#' in a data.table.
#'
#' To include standardized beta(s) in the regression results table,
#' the following package(s) must be installed prior to running the function:
#' Package 'lm.beta' v1.5-1 (or possibly a higher version) by
#' Stefan Behrendt (2014),
#' <https://cran.r-project.org/package=lm.beta>
#'
#' @param data a data object (a data frame or a data.table)
#' @param formula a formula object for the regression equation
#' @param sigfigs number of significant digits to round to
#' @param round_digits_after_decimal round to nth digit after decimal
#' (alternative to \code{sigfigs})
#' @return the output will be a data.table showing multiple regression
#' results.
#' @examples multiple_regression(data = mtcars, formula = mpg ~ gear * cyl)
#' @export
multiple_regression <- function(
  data = NULL,
  formula = NULL,
  sigfigs = NULL,
  round_digits_after_decimal = NULL) {
  # check if Package 'lm.beta' is installed
  if (!"lm.beta" %in% rownames(utils::installed.packages())) {
    message(paste0(
      "To include standardized betas, Package 'lm.beta' must ",
      "be installed.\nTo install Package 'lm.beta', type ",
      "'kim::prep(lm.beta)'",
      "\n\nAlternatively, to install all packages (dependencies) required ",
      "for all\nfunctions in Package 'kim', type ",
      "'kim::install_all_dependencies()'"))
  } else {
    # proceed if Package 'lm.beta' is already installed
    lm_beta_fn_from_lm_beta <- utils::getFromNamespace(
      "lm.beta", "lm.beta")
  }
  # regression model
  model <- stats::lm(formula = formula, data = data)
  model_summary <- summary(model)
  # get the results
  reg_results <- model_summary[["coefficients"]]
  variable <- row.names(reg_results)
  estimate <- reg_results[, "Estimate"]
  se <- reg_results[, "Std. Error"]
  t_stat <- reg_results[, "t value"]
  p_value <- kim::pretty_round_p_value(reg_results[, "Pr(>|t|)"])
  r_squared <- model_summary[["r.squared"]]
  adj_r_squared <- model_summary[["adj.r.squared"]]
  df_model <- model_summary[["fstatistic"]][["numdf"]]
  df_residual <- model_summary[["fstatistic"]][["dendf"]]
  f_stat <- model_summary[["fstatistic"]][["value"]]
  model_p_value <- kim::pretty_round_p_value(
    stats::pf(f_stat, df_model, df_residual, lower.tail = FALSE),
    include_p_equals = TRUE
  )
  n <- nrow(stats::model.frame(model))
  # calculate standardized betas if lm.beta package is already installed
  if ("lm.beta" %in% rownames(utils::installed.packages())) {
    std_beta <- lm_beta_fn_from_lm_beta(model)[[
      "standardized.coefficients"]]
  } else {
    std_beta <- rep(NA, length(estimate))
  }
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
