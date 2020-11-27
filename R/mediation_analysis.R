#' Mediation analysis
#'
#' Conducts a mediation analysis to estimate an independent variable's
#' indirect effect on dependent variable through a mediator variable.
#' The current version of the package only supports a simple mediation
#' model consisting of one independent variable, one mediator variable,
#' and one dependent variable.
#' Uses the source code from 'mediation' package v4.5.0,
#' Tingley et al. (2019)
#' <https://cran.r-project.org/web/packages/mediation/index.html>
#'
#' @param data a data object (a data frame or a data.table)
#' @param iv_name name of the independent variable
#' @param mediator_name name of the mediator variable
#' @param dv_name name of the dependent variable
#' @param covariates_names names of covariates to control for
#' @param robust_se if \code{TRUE}, heteroskedasticity-consistent
#' standard errors will be used in quasi-Bayesian simulations. By default,
#' it will be set as \code{FALSE} if nonparametric bootstrap is used and
#' as \code{TRUE} if quasi-Bayesian approximation is used.
#' @param iterations number of bootstrap samples. The default is set at 1000,
#' but consider increasing the number of samples to 5000, 10000, or an
#' even larger number, if slower handling time is not an issue.
#' @examples
#' \donttest{
#' mediation_analysis(data = mtcars, iv_name = "cyl",
#' mediator_name = "disp", dv_name = "mpg", iterations = 100)
#' }
#'
#' @export
mediation_analysis <- function(
  data = NULL,
  iv_name = NULL,
  mediator_name = NULL,
  dv_name = NULL,
  covariates_names = NULL,
  robust_se = TRUE,
  iterations = 1000) {
  # check number of variables entered
  if (any(lengths(list(iv_name, mediator_name, dv_name)) > 1)) {
    stop(paste0(
      "The current version of the package can only handle only ",
      "one independent variable, one dependent variable, and ",
      "one mediator variable."))
  }
  if (iterations < 1000) {
    message(paste0(
      "\nThe number of bootstrapping samples you entered, iterations = ",
      iterations, ", seems too low.\n\nPlease consider using a ",
      "larger number of bootstrapping samples",
      "\n(e.g., 1000, 2000, 5000, or 10000) if you can tolerate a ",
      "slow running time.\n"))
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
  # x: full model after mediation analysis
  x <- mediation::mediate(
    med_model,
    outcome_model,
    sims = iterations,
    treat = iv_name,
    mediator = mediator_name,
    robustSE = robust_se)
  # table output, adapted from the mediation package v4.5.0 source code
  smat <- c(x$d1, x$d1.ci, x$d1.p)
  smat <- rbind(smat, c(x$z0, x$z0.ci, x$z0.p))
  smat <- rbind(smat, c(x$tau.coef, x$tau.ci, x$tau.p))
  smat <- rbind(smat, c(x$n0, x$n0.ci, x$n0.p))
  clp <- 100 * x$conf.level
  rownames(smat) <- c(
    "Indirect Effect", "Direct Effect", "Total Effect",
    "Proportion Mediated")
  colnames(smat) <- c(
    "Estimate", paste0(clp, "% CI Lower"),
    paste0(clp, "% CI Upper"), "p-value")
  # print mediation output
  cat(paste0(
    "\nMediation analysis using 'mediation' package ",
    "(Tingley et al. 2019; v4.5.0)\n\n",
    "Quasi-Bayesian Confidence Intervals\n\n"))
  stats::printCoefmat(smat, digits = 4)
  cat("\nSample Size Used:", x$nobs, "\n\n")
  cat("Simulations:", x$sims, "\n")
  invisible(x)
}
