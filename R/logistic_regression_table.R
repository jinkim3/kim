#' Logistic regression table
#'
#' Construct a table of logistic regression results from the given
#' glm object estimating a logistic regression model.
#'
#' @param logistic_reg_glm_object a glm object estimating a
#' logistic regression model
#' @param z_values_keep logical. Should the z values be kept in the table?
#' (default = FALSE)
#' @param constant_row_clean logical. Should the row for the constant
#' be cleared except for b and standard error of b? (default = TRUE)
#' @param odds_ratio_cols_combine logical. Should the odds ratio columns
#' be combined? (default = TRUE)
#' @param round_b_and_se number of decimal places to which to round
#' b and standard error of b (default = 3)
#' @param round_z number of decimal places to which to round
#' z values (default = 3)
#' @param round_p number of decimal places to which to round
#' p-values (default = 3)
#' @param round_odds_ratio number of decimal places to which to round
#' odds ratios (default = 3)
#' @param round_r_sq number of decimal places to which to round
#' R-squared values (default = 3)
#' @param round_model_chi_sq number of decimal places to which to round
#' model chi-squared values (default = 3)
#' @param pretty_round_p_value logical. Should the p-values be rounded
#' in a pretty format (i.e., lower threshold: "<.001").
#' By default, \code{pretty_round_p_value = TRUE}.
#' @return the output will be a summary of logistic regression results.
#' @examples
#' logistic_regression_table(logistic_reg_glm_object =
#' glm(formula = am ~ mpg, family = binomial(), data = mtcars))
#' logistic_regression_table(logistic_reg_glm_object =
#' glm(formula = am ~ mpg, family = binomial(), data = mtcars),
#' z_values_keep = TRUE, constant_row_clean = FALSE,
#' odds_ratio_cols_combine = FALSE)
#' @export
# logistic regression with one interaction term
logistic_regression_table <- function(
  logistic_reg_glm_object = NULL,
  z_values_keep = FALSE,
  constant_row_clean = TRUE,
  odds_ratio_cols_combine = TRUE,
  round_b_and_se = 3,
  round_z = 3,
  round_p = 3,
  round_odds_ratio = 3,
  round_r_sq = 3,
  round_model_chi_sq = 3,
  pretty_round_p_value = TRUE) {
  # bind the vars locally to the function
  z <- or <- or_ci_95_ll <- or_ci_95_ul <- var <- b <- se_b <- NULL
  p <- or_95_ci <- NULL
  # check whether the glm object was given
  if (inherits(logistic_reg_glm_object, "glm") == FALSE) {
    stop("The input for logistic_reg_glm_object must be a glm object.")
  }
  # store the glm object
  glm_1 <- logistic_reg_glm_object
  # model chi square
  glm_1_model_chi <-
    glm_1$null.deviance - glm_1$deviance
  glm_1_model_chi_df <-
    glm_1$df.null - glm_1$df.residual
  glm_1_model_chi_p <- 1 - stats::pchisq(
    glm_1_model_chi, glm_1_model_chi_df)
  # r squared measures
  r_sq_hosmer_lemeshow <- glm_1_model_chi /
    glm_1$null.deviance
  n <- nrow(stats::na.omit(glm_1$model))
  r_sq_cox_snell <- 1 - exp((
    glm_1$deviance - glm_1$null.deviance) / n)
  r_sq_nagelkerke <-
    r_sq_cox_snell / (1 - (exp(-(glm_1$null.deviance / n))))
  # elements for tabulated results
  logistic_reg_results <- data.table(
    names(glm_1$coefficients),
    summary(glm_1)$coefficients)
  names(logistic_reg_results) <- c("var", "b", "se_b", "z", "p")
  # drop the z column
  if (z_values_keep == FALSE) {
    logistic_reg_results[, z := NULL]
  }
  # add the odds ratio columns
  logistic_reg_results[
    , or := exp(glm_1$coefficients)]
  logistic_reg_results[
    , or_ci_95_ll := suppressMessages(
      exp(matrix(stats::confint(glm_1), ncol = 2)))[, 1]]
  logistic_reg_results[
    , or_ci_95_ul := suppressMessages(
      exp(matrix(stats::confint(glm_1), ncol = 2)))[, 2]]
  # combine or ci columns
  if (odds_ratio_cols_combine == TRUE) {
    logistic_reg_results[, or_95_ci := paste0(
      "[", round(or_ci_95_ll, round_odds_ratio),
      ", ", round(or_ci_95_ul, round_odds_ratio), "]")]
    logistic_reg_results[, paste0("or_ci_95_", c("ll", "ul")) := NULL]
  }
  # clean irrelevant cells in the constant row
  if (constant_row_clean == TRUE) {
    cols_to_clear_for_constant <- c(
      "p", "or", paste0("or_ci_95_", c("ll", "ul")), "or_95_ci")
    for (col in cols_to_clear_for_constant) {
      if (col %in% names(logistic_reg_results)) {
        data.table::set(
          logistic_reg_results,
          i = which(logistic_reg_results[, var] == "(Intercept)"),
          j = col,
          value = NA)
      }
    }
  }
  # change the label intercept to constant
  logistic_reg_results[var == "(Intercept)", var := "(constant)"]
  # round values
  logistic_reg_results[, b := round(b, round_b_and_se)]
  logistic_reg_results[, se_b := round(se_b, round_b_and_se)]
  if (z_values_keep == TRUE) {
    logistic_reg_results[, z := round(z, round_z)]
  }
  if (pretty_round_p_value == TRUE) {
    logistic_reg_results[, p := pretty_round_p_value(p, round_p)]
  } else {
    logistic_reg_results[, p := round(p, round_p)]
  }
  logistic_reg_results[, or := round(or, round_odds_ratio)]
  if (odds_ratio_cols_combine == FALSE) {
    logistic_reg_results[, or_ci_95_ll := round(
      or_ci_95_ll, round_odds_ratio)]
    logistic_reg_results[, or_ci_95_ul := round(
      or_ci_95_ul, round_odds_ratio)]
  }
  # chi squared p value
  if (pretty_round_p_value == TRUE) {
    glm_1_model_chi_p_text <- pretty_round_p_value(
      glm_1_model_chi_p)
  } else {
    glm_1_model_chi_p_text <- paste0("= ", round(
      glm_1_model_chi_p, round_p))
  }
  # add model stats
  model_note <- data.table::data.table(var = c(
    "",
    paste0("R-squared (Hosmer-Lemeshow) = ",
           round(r_sq_hosmer_lemeshow, round_r_sq)),
    paste0("R-squared (Cox-Snell) = ",
           round(r_sq_cox_snell, round_r_sq)),
    paste0("R-squared (Nagelkerke) = ",
           round(r_sq_nagelkerke, round_r_sq)),
    paste0("Model chi-squared (df = ",
           glm_1_model_chi_df, ") = ",
           round(glm_1_model_chi, round_model_chi_sq)),
    paste0("Model chi-squared p-value ",
           glm_1_model_chi_p_text)))
  logistic_reg_w_model_stats <- merge_data_tables(
    dt1 = logistic_reg_results,
    dt2 = model_note,
    id = "var")
  # change all cols to character
  for (j in seq_len(ncol(logistic_reg_w_model_stats))) {
    set(logistic_reg_w_model_stats,
        j = j, value = as.character(logistic_reg_w_model_stats[[j]]))
  }
  # replace na values
  for (j in seq_len(ncol(logistic_reg_w_model_stats))) {
    set(logistic_reg_w_model_stats,
        which(is.na(logistic_reg_w_model_stats[[j]])), j , "")
  }
  # return the dt
  return(logistic_reg_w_model_stats)
}
