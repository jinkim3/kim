#' Standardized Regression
#'
#' This function standardizes all variables for a regression analysis
#' (i.e., dependent variable and all independent variables) and then
#' conducts a regression with the standardized variables.
#'
#' @param data a data object (a data frame or a data.table)
#' @param formula a formula object for the regression equation
#' @param sigfigs number of significant digits to round to
#' @param round_digits_after_decimal round to nth digit after decimal
#' (alternative to \code{sigfigs})
#' @param pretty_round_p_value logical. Should the p-values be rounded
#' in a pretty format (i.e., lower threshold: "<.001").
#' By default, \code{pretty_round_p_value = TRUE}.
#' @param round_p number of decimal places to which to round p-values
#' (default = 3)
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
#' @return the output will be a data.table showing multiple regression
#' results.
#' @examples
#' \donttest{
#' standardized_regression(data = mtcars, formula = mpg ~ gear * cyl)
#' }
#' @export
standardized_regression <- function(
  data = NULL,
  formula = NULL,
  sigfigs = NULL,
  round_digits_after_decimal = NULL,
  round_p = 3,
  pretty_round_p_value = TRUE,
  return_table_upper_half = FALSE,
  round_r_squared = 3,
  round_f_stat = 2,
  prettify_reg_table_col_names = TRUE) {
  # check whether all variable names are unique
  all_vars <- all.vars(formula)
  if (any(duplicated(all_vars)) == TRUE) {
    kim::find_duplicates(all_vars)
    stop(paste0(
      "This function requires that\nall variable names in the formula",
      " be unique (see above)."))
  }
  # copy data
  dt <- data.table::setDT(data.table::copy(data))
  # subset to only the vars needed
  dt <- dt[, all_vars, with = FALSE]
  # remove na values
  dt <- stats::na.omit(dt)
  # deal with variables of different types
  # i.e., convert character or factor variables if they are binary
  invisible(lapply(all_vars, function(x) {
    if (is.character(dt[[x]]) == TRUE) {
      if (length(unique(dt[[x]])) != 2) {
        stop(paste0(
          "The variable, '", x, "' is a character variable, ",
          "but it has ", length(unique(dt[[x]])),
          " levels.\nPlease ensure that all character ",
          "variables have exactly 2 levels."))
      } else if (length(unique(dt[[x]])) == 2) {
        # old values for a conversion table
        old_value <- sort(unique(dt[[x]]))
        # convert the character variable to a binary variable
        data.table::set(dt, j = x, value = scale(fcase(
          dt[[x]] == sort(unique(dt[[x]]))[1], 0,
          dt[[x]] == sort(unique(dt[[x]]))[2], 1)))
        # new values for a conversion table
        new_value <- sort(unique(dt[[x]]))
        conversion_table <- data.table::data.table(
          old_value, new_value)
        # print the conversion table
        message(paste0(
          "The variable '", x, "' has been recoded as follows:"))
        print(conversion_table)
      }
    } else if (is.factor(dt[[x]]) == TRUE) {
      if (length(unique(dt[[x]])) != 2) {
        stop(paste0(
          "The variable, '", x, "' is a factor variable, ",
          "but it has ", length(unique(dt[[x]])),
          " levels.\nPlease ensure that all character ",
          "variables have exactly 2 levels."))
      } else if (length(unique(dt[[x]])) == 2) {
        # old values for a conversion table
        old_value <- levels(dt[[x]])
        # convert the character variable to a binary variable
        data.table::set(dt, j = x, value = scale(fcase(
          dt[[x]] == levels(dt[[x]])[1], 0,
          dt[[x]] == levels(dt[[x]])[2], 1)))
        # new values for a conversion table
        new_value <- sort(unique(dt[[x]]))
        conversion_table <- data.table::data.table(
          old_value, new_value)
        # print the conversion table
        message(paste0(
          "The variable '", x, "' has been recoded as follows:"))
        print(conversion_table)
      }
    } else if (is.numeric(dt[[x]]) == TRUE) {
      data.table::set(dt, j = x, value = scale(dt[[x]]))
    } else {
      stop(paste0(
        "For this function to run, the variable '", x,
        "' must be either\na numeric variable ",
        "or a character/factor variable with exactly two levels."))
    }
  }))
  # conduct a regression
  model <- stats::lm(formula = formula, data = dt)
  # regression model summary
  model_summary <- summary(model)
  # get the results
  reg_results <- model_summary[["coefficients"]]
  variable <- row.names(reg_results)
  # change intercept to constant
  if (variable[1] == "(Intercept)") {variable[1] <- "(Constant)"}
  std_beta <- reg_results[, "Estimate"]
  se_of_std_beta <- reg_results[, "Std. Error"]
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
    std_beta <- kim::round_flexibly(std_beta, sigfigs)
    se_of_std_beta <- kim::round_flexibly(se_of_std_beta, sigfigs)
    t_stat <- kim::round_flexibly(t_stat, sigfigs)
    p_value <- kim::round_flexibly(p_value, sigfigs)
    model_p_value <- kim::round_flexibly(model_p_value, sigfigs)
  }
  if (!is.null(round_digits_after_decimal)) {
    std_beta <- round(std_beta, round_digits_after_decimal)
    se_of_std_beta <- round(se_of_std_beta, round_digits_after_decimal)
    t_stat <- round(t_stat, round_digits_after_decimal)
    p_value <- round(p_value, round_digits_after_decimal)
    model_p_value <- round(model_p_value, round_digits_after_decimal)
  }
  # round figures in the first column of the regression table
  r_squared <- kim::pretty_round_r(r_squared, round_r_squared)
  adj_r_squared <- kim::pretty_round_r(adj_r_squared, round_r_squared)
  f_stat <- round(f_stat, round_f_stat)
  # pretty round p_value
  if (pretty_round_p_value == TRUE) {
    p_value <- kim::pretty_round_p_value(
      p_value_vector = p_value,
      round_digits_after_decimal = round_p)
  }
  # upper part of the regression table
  t1 <- data.table::data.table(
    variable, std_beta, se_of_std_beta, t_stat, p_value)
  # return only the upper part of the regression table
  if (return_table_upper_half == TRUE) {
    return(t1)
  }
  # add an empty row
  t2 <- rbind(t1, as.list(rep("", ncol(t1))))
  # pretty round model_p_value
  if (pretty_round_p_value == TRUE) {
    model_p_value_text <- kim::pretty_round_p_value(
      p_value_vector = model_p_value,
      round_digits_after_decimal = round_p,
      include_p_equals = TRUE)
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
  t3 <- data.table::data.table(
    variable = variable_2,
    std_beta = rep("", length(variable_2)),
    se_of_std_beta = rep("", length(variable_2)),
    t_stat = rep("", length(variable_2)),
    p_value = rep("", length(variable_2))
  )
  t4 <- rbind(t2, t3)
  # rename columns
  if (prettify_reg_table_col_names == TRUE) {
    data.table::setnames(
      t4, c("variable", "std_beta", "se_of_std_beta", "t_stat", "p_value"),
      c("Variable", "Std. Beta", "SE of Std. Beta", "t", "p"))
  }
  return(t4)
}
