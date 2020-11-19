#' Mediation analysis
#'
#' Conducts a mediation analysis to estimate an independent variable's
#' indirect effect on dependent variable through the mediator variable
#'
#' @param data a data object (a data frame or a data.table)
#' @param iv_name name of the independent variable
#' @param mediator_name name of the mediator variable
#' @param dv_name name of the dependent variable
#' @param covariates_names names of covariates to control for
#' @param robust_se if \code{TRUE}, heteroskedasticity-consistent
#' standard errors will be used. Please refer to "mediation" package
#' (Tingley et al., 2014)
#' @param iterations number of bootstrap samples. The default is set at 1000,
#' but consider increasing the number of samples to 5000, 10000, or an
#' even larger number, if slower handling time is not an issue.
#' @param plot if \code{TRUE}, a plot of indirect, direct, and
#' total effects' respective sizes will be printed.
#' @param output_type if \code{output_type = summary}, the output will be
#' a summary of the mediation anaylsis or if \code{output_type = model},
#' the output will be a mediation model object. The default value
#' is "summary".
#' @examples
#' \donttest{mediation_analysis(data = mtcars, iv_name = "cyl",
#' mediator_name = "disp", dv_name = "mpg", iterations = 100)}
#' @export
mediation_analysis <- function(
  data = NULL,
  iv_name = NULL,
  mediator_name = NULL,
  dv_name = NULL,
  covariates_names = NULL,
  robust_se = TRUE,
  iterations = 1000,
  plot = TRUE,
  output_type = "summary") {
  if (iterations < 1000) {
    message(paste0(
      "The number of bootstrapping samples you entered, iterations = ",
      iterations, " seems too low.\nPlease consider using a ",
      "larger number of bootstrapping samples ",
      "(e.g., 1000, 2000, 5000, or 10000) if you can tolerate a",
      "slow running time."))
  }
  med_model_formula <- stats::as.formula(paste0(
    mediator_name, " ~ ", iv_name))
  outcome_model_formula <- stats::as.formula(paste0(
    dv_name, " ~ ", mediator_name, " + ", iv_name))
  if (!is.null(covariates_names)) {
    med_model_formula <- paste0(
      mediator_name, " ~ ", iv_name, " + ",
      paste0(covariates_names, collapse = " + "))
    outcome_model_formula <- paste0(
      dv_name, " ~ ", mediator_name, " + ", iv_name, " + ",
      paste0(covariates_names, collapse = " + "))
  }
  med_model <- stats::lm(
    formula = med_model_formula,
    data = data)
  outcome_model <- stats::lm(
    formula = outcome_model_formula, data = data)
  full_model <- mediation::mediate(
    med_model,
    outcome_model,
    treat = iv_name,
    mediator = mediator_name,
    robustSE = robust_se,
    sims = iterations)
  mediation_analysis_summary <- summary(full_model)
  if (plot == TRUE) {
    plot_output <- plot(full_model)
    print(plot_output)
  }
  print(mediation_analysis_summary)
  if (output_type == "summary") {
    output <- mediation_analysis_summary
  } else if (output_type == "model") {
    output <- full_model
  }
  invisible(output)
}
