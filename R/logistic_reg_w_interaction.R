#' Logistic regression with an interaction term
#'
#' Conduct logistic regression for a model with an interaction between
#' two predictor variables
#'
#' @param data a data object (a data frame or a data.table)
#' @param dv_name name of the dependent variable (must be a binary variable)
#' @param iv_1_name name of the first independent variable
#' @param iv_2_name name of the second independent variable
#' @return the output will be a data.table object (summary).
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
  iv_2_name = NULL) {
  # model 1
  logistic_reg_model_1_formula <-
    as.formula(paste0(
      dv_name, " ~ ", iv_1_name, " + ", iv_2_name))
  logistic_reg_model_1 <-
    stats::glm(
      logistic_reg_model_1_formula, data = data, family = binomial())
  # model 2
  logistic_reg_model_2_formula <-
    as.formula(paste0(
      dv_name, " ~ ", iv_1_name, " * ", iv_2_name))
  logistic_reg_model_2 <-
    stats::glm(
      logistic_reg_model_2_formula, data = data, family = binomial())
  # print
  print(summary(logistic_reg_model_1))
  print(summary(logistic_reg_model_2))
  # model_chi
  model_chi <- logistic_reg_model_1$deviance -
    logistic_reg_model_2$deviance
  chidf <- logistic_reg_model_1$df.residual -
    logistic_reg_model_2$df.residual
  # chi square probability
  chisq.prob <- 1 - pchisq(model_chi, chidf)
  message(paste0(
    "chi-square (df = ", chidf, ")", " = ", round(model_chi, 2)))
  message(paste0(
    "chi-square p-value = ", pretty_round_p_value(chisq.prob, 3)))
  # result about the interaction term
  if(chisq.prob >= .05) {
    message(paste0(
      "The interaction term did not singificantly improve the model fit, ",
      "p = ", pretty_round_p_value(chisq.prob, 3), "."))
  } else {
    message(paste0(
      "The interaction term singificantly improved the model fit, ",
      "p = ", pretty_round_p_value(chisq.prob, 3), "."))
  }
}
