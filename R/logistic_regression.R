#' Logistic regression
#'
#' Conduct a logistic regression analysis
#'
#' @param data a data object (a data frame or a data.table)
#' @param formula formula for estimating a single logistic regression model
#' @param formula_1 formula for estimating logistic regression model 1 of 2
#' @param formula_2 formula for estimating logistic regression model 2 of 2
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
#' @param print_glm_default_summary logical. Should the default summary
#' output of the glm objects be printed? (default = FALSE)
#' @param print_summary_dt_list logical. Should the summaries of
#' logistic regressions in a data table format be printed? (default = TRUE)
#' @param print_model_comparison logical. Should the comparison of
#' two logistic regression models be printed? (default = TRUE)
#' @param output_type If \code{output_type = "summary_dt_list"} (default),
#' the output of the function will be summaries of the two logistic
#' regressions in a data.table format.
#' If \code{output_type = "glm_object_list"}, the output of the
#' function will be the two \code{glm} objects estimating logistic
#' regression models. If \code{output_type = "glm_default_summary_list"},
#' the output of the function will be the R's default \code{summary}
#' output for the two \code{glm} objects estimating logistic
#' regression models. If \code{output_type = "model_comparison_stats"},
#' the output of the function will be statistics from comparison of the
#' two logistic regression models. If \code{output_type = "all"},
#' the output of the function will be a list of the aforementioned outputs.
#' @return the output will be a summary of logistic regression results,
#' unless set otherwise by the \code{output_type} argument to the function.
#' @examples
#' logistic_regression(data = mtcars, formula = am ~ mpg)
#' logistic_regression(
#' data = mtcars,
#' formula_1 = am ~ mpg,
#' formula_2 = am ~ mpg + wt)
#' @export
# logistic regression with one interaction term
logistic_regression <- function(
  data = NULL,
  formula = NULL,
  formula_1 = NULL,
  formula_2 = NULL,
  z_values_keep = FALSE,
  constant_row_clean = TRUE,
  odds_ratio_cols_combine = TRUE,
  round_b_and_se = 3,
  round_z = 3,
  round_p = 3,
  round_odds_ratio = 3,
  round_r_sq = 3,
  round_model_chi_sq = 3,
  pretty_round_p_value = TRUE,
  print_glm_default_summary = FALSE,
  print_summary_dt_list = TRUE,
  print_model_comparison = TRUE,
  output_type = "summary_dt_list") {
  # copy data
  dt <- data.table::setDT(data.table::copy(data))
  # check formula
  if (!is.null(formula)) {
    if (inherits(formula, "formula") != TRUE) {
      stop(paste0("The input for formula is not a valid formula."))
    } else if (inherits(formula, "formula") == TRUE) {
      # check if formula_1 and formula_2 received any inputs
      if (!is.null(formula_1) | !is.null(formula_2)) {
        kim::pm(
          'Because the "formula" argument received an input, ',
          'any input(s) given for the "formula_1" or "formula_2" ',
          "arguments will be ignored.")
      }
      # how many formulas were given?
      num_of_formulas_given <- 1
    }
  } else {
    # check fomula 1
    if (inherits(formula_1, "formula") != TRUE) {
      stop(paste0("The input for formula_1 is not a valid formula."))
    }
    # check fomula 2
    if (inherits(formula_2, "formula") != TRUE) {
      stop(paste0("The input for formula_2 is not a valid formula."))
    }
    # how many formulas were given?
    num_of_formulas_given <- 2
  }
  # valid formulas
  if (num_of_formulas_given == 1) {
    valid_formula_1 <- stats::as.formula(paste(formula[2], "~ 1"))
    valid_formula_2 <- formula
  }
  if (num_of_formulas_given == 2) {
    valid_formula_1 <- formula_1
    valid_formula_2 <- formula_2
  }
  # logisitic regression for valid_formula_1
  logistic_reg_models <- list()
  logistic_reg_models[[1]] <- stats::glm(
    formula = valid_formula_1,
    family = stats::binomial(),
    data = dt)
  logistic_reg_models[[2]] <- stats::glm(
    formula = valid_formula_2,
    family = stats::binomial(),
    data = dt)
  # return glm default summary
  if (output_type == "glm_object_list") {
    return(logistic_reg_models)
  }
  # print logistic regression models
  if (print_glm_default_summary == TRUE) {
    glm_default_summary_list <- lapply(logistic_reg_models, summary)
    glm_default_summary_list
  }
  # return glm default summary
  if (output_type == "glm_default_summary_list") {
    return(glm_default_summary_list)
  }
  # nicer table
  summary_dt_list <- lapply(logistic_reg_models, function(x) {
    kim::logistic_regression_table(
      logistic_reg_glm_object = x,
      z_values_keep = z_values_keep,
      constant_row_clean = constant_row_clean,
      odds_ratio_cols_combine = odds_ratio_cols_combine,
      round_b_and_se = round_b_and_se,
      round_z = round_z,
      round_p = round_p,
      round_odds_ratio = round_odds_ratio,
      round_r_sq = round_r_sq,
      round_model_chi_sq = round_model_chi_sq,
      pretty_round_p_value = pretty_round_p_value)
  })
  # model labels
  model_labels <- c(
    paste0("Model 1: ", paste(
      valid_formula_1[2], valid_formula_1[3], sep = " ~ ")),
    paste0("Model 2: ", paste(
      valid_formula_2[2], valid_formula_2[3], sep = " ~ ")))
  # label the summary dt list
  names(summary_dt_list) <- model_labels
  # print cleaned summary dt list
  if (print_summary_dt_list == TRUE) {
    print(summary_dt_list)
  }
  # compare models
  model_chi <- logistic_reg_models[[1]]$deviance -
    logistic_reg_models[[2]]$deviance
  model_chi_df <- logistic_reg_models[[1]]$df.residual -
    logistic_reg_models[[2]]$df.residual
  model_chi_p <- 1 - stats::pchisq(model_chi, model_chi_df)
  # print model comparison
  if (print_model_comparison == TRUE) {
    # print summary
    cat(model_labels[1])
    cat("\n")
    cat(model_labels[2])
    cat("\n")
    cat(paste0(
      "chi-squared (df = ", model_chi_df, ")", " = ",
      round(model_chi, round_model_chi_sq)))
    cat("\n")
    cat(paste0(
      "chi-squared ", kim::pretty_round_p_value(
        model_chi_p, round_p, include_p_equals = TRUE)))
    cat("\n")
    kim::pm(
      "Model 2 is ", ifelse(model_chi_p < 0.05, "", "not "),
      "a significant improvement over Model 1.")
  }
  # return model chi
  if (output_type == "model_comparison_stats") {
    model_comparison_stats <- list(
      model_chi = model_chi,
      model_chi_df = model_chi_df,
      model_chi_p = model_chi_p)
    return(model_comparison_stats)
  }
  # return cleaned summary dt list
  if (output_type == "summary_dt_list") {
    invisible(summary_dt_list)
  }
  # return all
  if (output_type == "all") {
    output <- list(
      glm_object_list = logistic_reg_models,
      glm_default_summary_list = glm_default_summary_list,
      summary_dt_list = summary_dt_list,
      model_comparison_stats = model_comparison_stats)
    return(output)
  }
}
