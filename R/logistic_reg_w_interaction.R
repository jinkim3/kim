#' Logistic regression with an interaction term
#'
#' Conduct logistic regression for a model with an interaction between
#' two predictor variables
#'
#' @param data a data object (a data frame or a data.table)
#' @param dv_name name of the dependent variable (must be a binary variable)
#' @param iv_1_name name of the first independent variable
#' @param iv_2_name name of the second independent variable
#' @param round_p number of decimal places to which to round
#' p-values (default = 3)
#' @param round_chi_sq number of decimal places to which to round
#' chi square statistics (default = 2)
#' @param dv_ordered_levels a vector with the ordered levels of the
#' dependent variable, the first and second elements of which will be
#' coded as 0 and 1, respectively, to run logistic regression.
#' E.g., \code{dv_ordered_levels = c("fail", "pass")}
#' @param iv_1_ordered_levels (only if the first independent variable
#' is a binary variable) a vector with the ordered levels of the first
#' independent variable, the first and second elements of which will be
#' coded as 0 and 1, respectively, to run logistic regression.
#' E.g., \code{iv_1_ordered_levels = c("control", "treatment")}
#' @param iv_2_ordered_levels (only if the second independent variable
#' is a binary variable) a vector with the ordered levels of the first
#' independent variable, the first and second elements of which will be
#' coded as 0 and 1, respectively, to run logistic regression.
#' E.g., \code{iv_2_ordered_levels = c("male", "female")}
#' @param one_line_summary_only logical. Should the output simply be a
#' printout of a one-line summary on the interaction term? (default = FALSE)
#' @param p_value_interaction_only logical. Should the output simply be a
#' p-value of the interaction term in the logistic regression model?
#' (default = FALSE)
#' @param return_dt_w_binary logical. If \code{return_dt_w_binary = TRUE},
#' the function will return a data.table with binary variables coded
#' as 0 or 1 (default = FALSE)
#' @return the output will be a summary of logistic regression results,
#' unless set otherwise by arguments to the function.
#' @examples
#' logistic_reg_w_interaction(data = mtcars, dv_name = "vs",
#' iv_1_name = "mpg", iv_2_name = "am")
#' @import data.table
#' @export
# logistic regression with one interaction term
logistic_reg_w_interaction <- function(
  data = NULL,
  dv_name = NULL,
  iv_1_name = NULL,
  iv_2_name = NULL,
  round_p = 3,
  round_chi_sq = 2,
  dv_ordered_levels = NULL,
  iv_1_ordered_levels = NULL,
  iv_2_ordered_levels = NULL,
  one_line_summary_only = FALSE,
  p_value_interaction_only = FALSE,
  return_dt_w_binary = FALSE) {
  # copy data
  dt <- data.table::setDT(data.table::copy(data))
  # get unique values in dv
  dv <- dt[[dv_name]]
  dv_unique_values <- kim::su(dv, na.last = NA)
  # check whether the dv is binary
  if (length(dv_unique_values) != 2) {
    stop("The DV must have only two levels.")
  }
  # check whether the dv is coded as 0 or 1, and if not, coerce it
  if (all(dv_unique_values %in% 0:1) == FALSE) {
    if (is.null(dv_ordered_levels)) {
      dv_ordered_levels <- dv_unique_values
    }
    # recode dv
    data.table::set(dt, j = dv_name, value = data.table::fcase(
      dt[[dv_name]] == dv_ordered_levels[1], 0,
      dt[[dv_name]] == dv_ordered_levels[2], 1))
    # notify of the dv recoding
    if (one_line_summary_only == FALSE &
        p_value_interaction_only == FALSE) {
      message(
        "To run a logistic regression, the dv was recoded as follows:")
      print(data.table::data.table(old = dv_ordered_levels, new = 0:1))
    }
  }
  # get unique values in iv_1 and iv_2
  iv_1 <- dt[[iv_1_name]]
  iv_1_unique_values <- kim::su(iv_1, na.last = NA)
  iv_2 <- dt[[iv_2_name]]
  iv_2_unique_values <- kim::su(iv_2, na.last = NA)
  # check whether the first iv is binary, and if so,
  # coerce it to 0 or 1, if necessary
  if (length(iv_1_unique_values) == 2 &
      all(iv_1_unique_values %in% 0:1) == FALSE) {
    if (is.null(iv_1_ordered_levels)) {
      iv_1_ordered_levels <- iv_1_unique_values
    }
    # recode dv
    data.table::set(dt, j = iv_1_name, value = data.table::fcase(
      dt[[iv_1_name]] == iv_1_ordered_levels[1], 0,
      dt[[iv_1_name]] == iv_1_ordered_levels[2], 1))
    # notify of the iv_1 recoding
    if (one_line_summary_only == FALSE &
        p_value_interaction_only == FALSE) {
      message(
        "To run a logistic regression, the iv_1 was recoded as follows:")
      print(data.table::data.table(old = iv_1_ordered_levels, new = 0:1))
    }
  }
  # check whether the second iv is binary, and if so,
  # coerce it to 0 or 1, if necessary
  if (length(iv_2_unique_values) == 2 &
      all(iv_2_unique_values %in% 0:1) == FALSE) {
    if (is.null(iv_2_ordered_levels)) {
      iv_2_ordered_levels <- iv_2_unique_values
    }
    # recode dv
    data.table::set(dt, j = iv_2_name, value = data.table::fcase(
      dt[[iv_2_name]] == iv_2_ordered_levels[1], 0,
      dt[[iv_2_name]] == iv_2_ordered_levels[2], 1))
    # notify of the iv_2 recoding
    if (one_line_summary_only == FALSE &
        p_value_interaction_only == FALSE) {
      message(
        "To run a logistic regression, the iv_2 was recoded as follows:")
      print(data.table::data.table(old = iv_2_ordered_levels, new = 0:1))
    }
  }
  # return the current data table with the binary variables recoded
  if (return_dt_w_binary == TRUE) {
    return(dt)
  }
  # model 1
  logistic_reg_model_1_formula <-
    stats::as.formula(paste0(
      dv_name, " ~ ", iv_1_name, " + ", iv_2_name))
  logistic_reg_model_1 <-
    stats::glm(
      logistic_reg_model_1_formula, data = dt, family = stats::binomial())
  # model 2
  logistic_reg_model_2_formula <-
    stats::as.formula(paste0(
      dv_name, " ~ ", iv_1_name, " * ", iv_2_name))
  logistic_reg_model_2 <-
    stats::glm(
      logistic_reg_model_2_formula, data = dt, family = stats::binomial())
  # print
  if (one_line_summary_only == FALSE &
      p_value_interaction_only == FALSE) {
    print(summary(logistic_reg_model_1))
    print(summary(logistic_reg_model_2))
  }
  # model_chi
  model_chi <- logistic_reg_model_1$deviance -
    logistic_reg_model_2$deviance
  chidf <- logistic_reg_model_1$df.residual -
    logistic_reg_model_2$df.residual
  # chi square probability
  chisq_p <- 1 - stats::pchisq(model_chi, chidf)
  if (one_line_summary_only == FALSE &
      p_value_interaction_only == FALSE) {
    message(paste0(
      "chi-square (df = ", chidf, ")", " = ",
      round(model_chi, round_chi_sq)))
    message(paste0(
      "chi-square p-value = ", kim::pretty_round_p_value(
        chisq_p, round_p)))
  }
  # result about the interaction term
  if (p_value_interaction_only == FALSE) {
    if (chisq_p >= .05) {
      message(paste0(
        "The interaction term did not significantly improve the model fit, ",
        "p = ", kim::pretty_round_p_value(chisq_p, round_p), "."))
    } else {
      message(paste0(
        "The interaction term significantly improved the model fit, ",
        "p = ", kim::pretty_round_p_value(chisq_p, round_p), "."))
    }
  } else if (p_value_interaction_only == TRUE) {
    return(chisq_p)
  }
}
