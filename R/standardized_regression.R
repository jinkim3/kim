#' Standardized Regression
#'
#' This function standardizes all variables for a regression analysis
#' (i.e., dependent variable and all independent variables) and then
#' conducts a regression with the standardized variables.
#'
#' @param data a data object (a data frame or a data.table)
#' @param formula a formula object for the regression equation
#' @param reverse_code_vars names of binary variables to reverse code
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
#' standardized_regression(
#' data = mtcars, formula = mpg ~ gear + gear:am + disp * cyl,
#' round_digits_after_decimal = 3)
#' }
#' @export
standardized_regression <- function(
    data = NULL,
    formula = NULL,
    reverse_code_vars = NULL,
    sigfigs = NULL,
    round_digits_after_decimal = NULL,
    round_p = 3,
    pretty_round_p_value = TRUE,
    return_table_upper_half = FALSE,
    round_r_squared = 3,
    round_f_stat = 2,
    prettify_reg_table_col_names = TRUE) {
  # check whether all variable names are unique
  all_vars_in_entered_formula <- all.vars(formula)
  if (any(duplicated(all_vars_in_entered_formula)) == TRUE) {
    kim::find_duplicates(all_vars_in_entered_formula)
    stop(paste0(
      "This function requires that\nall variable names in the formula",
      " be unique (see above)."))
  }
  # check whether colon is included in any variable name
  if (any(grepl("\\:", all_vars_in_entered_formula)) == TRUE) {
    stop(
      "This function requires that no variable name contains a colon ':'")
  }
  # copy data
  dt <- data.table::setDT(data.table::copy(data))
  # subset to only the vars needed
  dt <- dt[, all_vars_in_entered_formula, with = FALSE]
  # remove na values
  dt <- stats::na.omit(dt)
  # get the righthand side terms
  rhs_terms <- labels(stats::terms(formula))
  # get the lefthand side term
  lhs_term <- all.vars(formula)[1]
  # get all vars to be used in regression
  all_vars_in_reg <- c(lhs_term, rhs_terms)
  # lists to store old and new values
  old_values <- new_values <- list()
  # deal with variables of different types
  # i.e., convert character or factor variables if they are binary
  for (i in seq_along(all_vars_in_entered_formula)) {
    x <- all_vars_in_entered_formula[i]
    if (is.character(dt[[x]]) == TRUE) {
      if (length(unique(dt[[x]])) != 2) {
        stop(paste0(
          "The variable, '", x, "' is a character variable, ",
          "but it has ", length(unique(dt[[x]])),
          " levels.\nPlease ensure that all character ",
          "variables have exactly 2 levels."))
      } else if (length(unique(dt[[x]])) == 2) {
        # order the values
        if (x %in% reverse_code_vars == FALSE) {
          ordered_old_values <- sort(unique(dt[[x]]))
        } else {
          # reverse code
          ordered_old_values <- rev(sort(unique(dt[[x]])))
        }
        # old values for a conversion table
        old_values[[x]] <- ordered_old_values
        # convert the character variable to a binary variable
        data.table::set(dt, j = x, value = fcase(
          dt[[x]] == ordered_old_values[1], 0,
          dt[[x]] == ordered_old_values[2], 1))
      }
    } else if (is.factor(dt[[x]]) == TRUE) {
      if (length(unique(dt[[x]])) != 2) {
        stop(paste0(
          "The variable, '", x, "' is a factor variable, ",
          "but it has ", length(unique(dt[[x]])),
          " levels.\nPlease ensure that all character ",
          "variables have exactly 2 levels."))
      } else if (length(unique(dt[[x]])) == 2) {
        # order the values
        if (x %in% reverse_code_vars == FALSE) {
          ordered_old_values <- levels(dt[[x]])
        } else {
          # reverse code
          ordered_old_values <- rev(levels(dt[[x]]))
        }
        # old values for a conversion table
        old_values[[x]] <- ordered_old_values
        # convert the character variable to a binary variable
        data.table::set(dt, j = x, value = fcase(
          dt[[x]] == ordered_old_values[1], 0,
          dt[[x]] == ordered_old_values[2], 1))
      }
    } else if (is.numeric(dt[[x]]) == FALSE) {
      stop(paste0(
        "For this function to run, the variable '", x,
        "' must be either\na numeric variable ",
        "or a character/factor variable with exactly two levels."))
    }
  }
  # a data table to store vars for standardizing
  dt2 <- data.table::data.table()
  # a vector to store temporary names for dealing with product terms
  # that contain the colon in their names
  all_vars_in_reg_temp_names <- rep(NA, length(all_vars_in_reg))
  # get all vars ready for standardization
  for (i in seq_along(all_vars_in_reg)) {
    # check if the term is a product
    if (grepl("\\:", all_vars_in_reg[i]) == TRUE) {
      # give a new, temporary name for the product term
      temp_name <- gsub("\\:", "__times__", all_vars_in_reg[i])
      all_vars_in_reg_temp_names[i] <- temp_name
      terms_for_product <- unlist(strsplit(
        all_vars_in_reg[i], ":"))
      data.table::set(dt2, j = temp_name, value = Reduce(
        "*", as.list(dt[, terms_for_product, with = FALSE])))
    } else {
      data.table::set(
        dt2, j = all_vars_in_reg[i], value = dt[[all_vars_in_reg[i]]])
      all_vars_in_reg_temp_names[i] <- all_vars_in_reg[i]
    }
  }
  # standardize all variables
  for (j in names(dt2)) {
    set(dt2, j = j, value = scale(dt2[[j]]))
  }
  # formula that uses only the plus signs
  formula_2 <- stats::as.formula(paste0(
    all_vars_in_reg_temp_names[1], " ~ ",
    paste0(utils::tail(
      all_vars_in_reg_temp_names, -1), collapse = " + ")))
  # conduct a regression
  model <- stats::lm(formula = formula_2, data = dt2)
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
  # correct variable names
  all_vars_in_reg_temp_names[1] <- "(Constant)"
  if (all(t1[["variable"]] == all_vars_in_reg_temp_names)) {
    all_vars_in_reg[1] <- "(Constant)"
    t1[["variable"]] <- all_vars_in_reg
  }
  # print conversion tables
  if (length(old_values) > 0) {
    vars_for_printing_conv_table <- names(old_values)
    for (x in vars_for_printing_conv_table) {
      if (length(old_values[[x]]) == length(unique(dt2[[x]]))) {
        conversion_table <- data.table::data.table(
          old_value = old_values[[x]],
          new_value = sort(unique(dt2[[x]])))
        message(paste0(
          "The variable '", x, "' has been recoded as follows:"))
        print(conversion_table)
      } else {
        stop(paste0(
          "An error seems to have occurred while recoding variable '",
          x, "'.\nPlease run the function again after manually coding ",
          "the variable."))
      }
    }
  }
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
