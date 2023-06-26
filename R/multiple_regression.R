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
#' @param vars_to_mean_center (deprecated) a character vector specifying names
#' of variables that will be mean-centered before the regression model
#' is estimated
#' @param mean_center_vars a character vector specifying names
#' of variables that will be mean-centered before the regression model
#' is estimated
#' @param sigfigs number of significant digits to round to
#' @param round_digits_after_decimal round to nth digit after decimal
#' (alternative to \code{sigfigs})
#' @param round_p number of decimal places to round p values
#' (overrides all other rounding arguments)
#' @param pretty_round_p_value logical. Should the p-values be rounded
#' in a pretty format (i.e., lower threshold: "<.001").
#' By default, \code{pretty_round_p_value = TRUE}.
#' @param return_table_upper_half logical. Should only the upper part
#' of the table be returned?
#' By default, \code{return_table_upper_half = FALSE}.
#' @param round_r_squared number of digits after the decimal both r-squared
#' and adjusted r-squared values should be rounded to (default 3)
#' @param round_f_stat number of digits after the decimal the f statistic
#' of the regression model should be rounded to (default 2)
#' @param prettify_reg_table_col_names logical. Should the column names
#' of the regression table be made pretty (e.g., change "std_beta" to
#' "Std. Beta")? (Default = \code{TRUE})
#' @param silent If \code{silent = FALSE}, a message regarding
#' mean-centered variables will be printed. If \code{silent = TRUE},
#' this message will be suppressed. By default, \code{silent = FALSE}.
#' @return the output will be a data.table showing multiple regression
#' results.
#' @examples
#' \donttest{
#' multiple_regression(data = mtcars, formula = mpg ~ gear * cyl)
#' multiple_regression(
#' data = mtcars, formula = mpg ~ gear * cyl,
#' mean_center_vars = "gear",
#' round_digits_after_decimal = 2)
#' }
#' @export
multiple_regression <- function(
  data = NULL,
  formula = NULL,
  vars_to_mean_center = NULL,
  mean_center_vars = NULL,
  sigfigs = NULL,
  round_digits_after_decimal = NULL,
  round_p = NULL,
  pretty_round_p_value = TRUE,
  return_table_upper_half = FALSE,
  round_r_squared = 3,
  round_f_stat = 2,
  prettify_reg_table_col_names = TRUE,
  silent = FALSE) {
  # installed packages
  installed_pkgs <- rownames(utils::installed.packages())
  # check if Package 'lm.beta' is installed
  if (!"lm.beta" %in% installed_pkgs) {
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
  # warning about deprecated argument
  if (!is.null(vars_to_mean_center)) {
    warning(paste0(
      "The argument 'vars_to_mean_center' is deprecated.\n",
      "Please use 'mean_center_vars' instead."))
    mean_center_vars <- vars_to_mean_center
  }
  # mean center vars
  if (!is.null(mean_center_vars)) {
    mean_center_vars_missing_in_data <- setdiff(
      mean_center_vars, names(data))
    if (length(mean_center_vars_missing_in_data) > 0) {
      stop(paste0(
        "The following variable(s) for mean-centering ",
        "do not exist in the data set:\n",
        paste0(mean_center_vars_missing_in_data, collapse = "\n")))
    }
    # copy data
    dt <- data.table::setDT(data.table::copy(data))
    # remove rows with na values in the key vars
    dt <- stats::na.omit(dt, cols = all.vars(formula))
    # mean center vars
    for (col in mean_center_vars) {
      data.table::set(
        dt, j = col, value = scale(dt[[col]], scale = FALSE))
    }
    # print a summary of mean centering
    if (silent == FALSE) {
      kim::pm(
        "The following variable(s) were mean-centered prior to ",
        "the regression analysis:\n",
        paste0(mean_center_vars, collapse = "\n"))
    }
    # regression model after mean centering
    model <- stats::lm(formula = formula, data = dt)
  } else {
    # regression model without mean centering
    model <- stats::lm(formula = formula, data = data)
  }
  # regression model summary
  model_summary <- summary(model)
  # get the results
  reg_results <- model_summary[["coefficients"]]
  variable <- row.names(reg_results)
  # change intercept to constant
  if (variable[1] == "(Intercept)") {variable[1] <- "(Constant)"}
  estimate <- reg_results[, "Estimate"]
  se <- reg_results[, "Std. Error"]
  t_stat <- reg_results[, "t value"]
  p_value <- reg_results[, "Pr(>|t|)"]
  r_squared <- model_summary[["r.squared"]]
  adj_r_squared <- model_summary[["adj.r.squared"]]
  df_model <- model_summary[["fstatistic"]][["numdf"]]
  df_residual <- model_summary[["fstatistic"]][["dendf"]]
  f_stat <- model_summary[["fstatistic"]][["value"]]
  model_p_value <- stats::pf(
    f_stat, df_model, df_residual, lower.tail = FALSE)
  n <- nrow(stats::model.frame(model))
  # calculate standardized betas if lm.beta package is already installed
  if ("lm.beta" %in% installed_pkgs) {
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
    estimate <- kim::round_flexibly(estimate, sigfigs)
    se <- kim::round_flexibly(se, sigfigs)
    std_beta <- kim::round_flexibly(std_beta, sigfigs)
    t_stat <- kim::round_flexibly(t_stat, sigfigs)
    p_value_rounded <- kim::round_flexibly(p_value, sigfigs)
    model_p_value <- kim::round_flexibly(model_p_value, sigfigs)
  }
  if (!is.null(round_digits_after_decimal)) {
    estimate <- round(estimate, round_digits_after_decimal)
    se <- round(se, round_digits_after_decimal)
    std_beta <- round(std_beta, round_digits_after_decimal)
    t_stat <- round(t_stat, round_digits_after_decimal)
    p_value_rounded <- round(p_value, round_digits_after_decimal)
    model_p_value <- round(model_p_value, round_digits_after_decimal)
  }
  # round p values only
  if (!is.null(round_p)) {
    p_value_rounded <- round(p_value, round_p)
  } else if (is.null(round_p)) {
    if (!is.null(sigfigs)) {
      round_p <- sigfigs
    }
    if (!is.null(round_digits_after_decimal)) {
      round_p <- round_digits_after_decimal
    }
  }
  # round figures in the first column of the regression table
  r_squared <- kim::pretty_round_r(r_squared, round_r_squared)
  adj_r_squared <- kim::pretty_round_r(adj_r_squared, round_r_squared)
  f_stat <- round(f_stat, round_f_stat)
  # pretty round p_value
  if (pretty_round_p_value == TRUE) {
    p_value_rounded <- kim::pretty_round_p_value(
      p_value, round_digits_after_decimal = round_p)
  }
  # upper part of the regression table
  t1 <- data.table::data.table(
    variable, estimate, se, std_beta, t_stat,
    p_value = p_value_rounded)
  # return only the upper part of the regression table
  if (return_table_upper_half == TRUE) {
    return(t1)
  }
  # add an empty row
  t2 <- rbind(t1, as.list(rep("", ncol(t1))))
  # pretty round model_p_value
  if (pretty_round_p_value == TRUE) {
    model_p_value_text <- kim::pretty_round_p_value(
      model_p_value, include_p_equals = TRUE)
  } else {
    model_p_value_text <- paste0("p = ", model_p_value)
  }
  # add model stats
  variable_2 <- c(
    paste0("R-squared: ", r_squared),
    paste0("Adj. R-squared: ", adj_r_squared),
    paste0("F(", df_model, ", ", df_residual, ") = ", f_stat),
    paste0("Model ", model_p_value_text),
    paste0("N = ", n),
    paste0("DV: ", all.vars(formula[[2]]))
  )
  # note the mean centered variables
  if (!is.null(mean_center_vars)) {
    variable_2 <- c(variable_2, paste0(
      "Mean-centered variable(s): ", paste0(
        mean_center_vars, collapse = ", ")))
  }
  t3 <- data.table::data.table(
    variable = variable_2,
    estimate = rep("", length(variable_2)),
    se = rep("", length(variable_2)),
    std_beta = rep("", length(variable_2)),
    t_stat = rep("", length(variable_2)),
    p_value = rep("", length(variable_2))
  )
  t4 <- rbind(t2, t3)
  # rename columns
  if (prettify_reg_table_col_names == TRUE) {
    data.table::setnames(
      t4, c("variable", "estimate", "se", "std_beta", "t_stat", "p_value"),
      c("Variable", "B", "SE B", "Std. Beta", "t", "p"))
  }
  return(t4)
}
