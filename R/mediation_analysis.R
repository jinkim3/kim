#' Mediation analysis
#'
#' Conducts a mediation analysis to estimate an independent variable's
#' indirect effect on dependent variable through a mediator variable.
#' The current version of the package only supports a simple mediation
#' model consisting of one independent variable, one mediator variable,
#' and one dependent variable.
#'
#' This function requires installing Package 'mediation' v4.5.0
#' (or possibly a higher version) by Tingley et al. (2019),
#' and uses the source code from a function in the package.
#' <https://cran.r-project.org/package=mediation>
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
#' @param output_type if \code{output_type = "summary_dt"},
#' return the summary data.table; if \code{output_type = "mediate_output"},
#' return the output from the \code{mediate} function in the
#' 'mediate' package; if \code{output_type = "indirect_effect_p"},
#' return the p value associated with the indirect effect estimated
#' in the mediation model (default = "summary_dt")
#' @param sigfigs number of significant digits to round to
#' @param silent if \code{silent = FALSE}, mediation analysis summary,
#' estimation method, sample size, and number of simulations will be
#' printed; if \code{silent = TRUE}, nothing will be printed.
#' (default = FALSE)
#' @return if \code{output_type = "summary_dt"}, which is the default,
#' the output will be a data.table showing a summary of mediation
#' analysis results; if \code{output_type = "mediate_output"},
#' the output will be the output from the \code{mediate} function
#' in the 'mediate' package; if \code{output_type = "indirect_effect_p"},
#' the output will be the p-value associated with the indirect effect
#' estimated in the mediation model (a numeric vector of length one).
#' @examples
#' \donttest{
#' mediation_analysis(
#'   data = mtcars, iv_name = "cyl",
#'   mediator_name = "disp", dv_name = "mpg", iterations = 100
#' )
#' mediation_analysis(
#'   data = iris, iv_name = "Sepal.Length",
#'   mediator_name = "Sepal.Width", dv_name = "Petal.Length",
#'   iterations = 100
#' )
#' }
#' @export
#' @import data.table
mediation_analysis <- function(
  data = NULL,
  iv_name = NULL,
  mediator_name = NULL,
  dv_name = NULL,
  covariates_names = NULL,
  robust_se = TRUE,
  iterations = 1000,
  sigfigs = 3,
  output_type = "summary_dt",
  silent = FALSE) {
  # check if Package 'mediation' is installed
  if (!"mediation" %in% rownames(utils::installed.packages())) {
    message(paste0(
      "To conduct a mediation analysis, Package 'mediation' must ",
      "be installed.\nTo install Package 'mediation', type ",
      "'kim::prep(mediation)'",
      "\n\nAlternatively, to install all packages (dependencies) required ",
      "for all\nfunctions in Package 'kim', type ",
      "'kim::install_all_dependencies()'"))
    return()
  } else {
    # proceed if Package 'mediation' is already installed
    mediate_fn_from_mediation <- utils::getFromNamespace(
      "mediate", "mediation")
  }
  # check number of variables entered
  if (any(lengths(list(iv_name, mediator_name, dv_name)) > 1)) {
    stop(paste0(
      "The current version of the package can only handle only ",
      "one independent variable, one dependent variable, and ",
      "one mediator variable."
    ))
  }
  if (iterations < 1000) {
    message(paste0(
      "\nThe number of bootstrapping samples you entered, iterations = ",
      iterations, ", seems too low.\n\nPlease consider using a ",
      "larger number of bootstrapping samples",
      "\n(e.g., 1000, 2000, 5000, or 10000) if you can tolerate a ",
      "slow running time.\n"
    ))
  }
  # omit na values
  dt <- data.table::setDT(data.table::copy(data))[
    !is.na(get(iv_name)) & !is.na(get(mediator_name)) &
      !is.na(get(dv_name))
  ]
  # build formulas
  med_model_formula <- stats::as.formula(paste0(
    mediator_name, " ~ ", iv_name
  ))
  outcome_model_formula <- stats::as.formula(paste0(
    dv_name, " ~ ", mediator_name, " + ", iv_name
  ))
  # add covariates
  if (!is.null(covariates_names)) {
    med_model_formula <- paste0(
      mediator_name, " ~ ", iv_name, " + ",
      paste0(covariates_names, collapse = " + ")
    )
    outcome_model_formula <- paste0(
      dv_name, " ~ ", mediator_name, " + ", iv_name, " + ",
      paste0(covariates_names, collapse = " + ")
    )
  }
  med_model <- stats::lm(
    formula = med_model_formula,
    data = dt
  )
  outcome_model <- stats::lm(
    formula = outcome_model_formula, data = dt
  )
  # x: full model after mediation analysis
  x <- mediate_fn_from_mediation(
    model.m = med_model,
    model.y = outcome_model,
    sims = iterations,
    treat = iv_name,
    mediator = mediator_name,
    robustSE = robust_se
  )
  # table output, adapted from the mediation package v4.5.0 source code
  smat <- c(x$d1, x$d1.ci, x$d1.p)
  smat <- rbind(smat, c(x$z0, x$z0.ci, x$z0.p))
  smat <- rbind(smat, c(x$tau.coef, x$tau.ci, x$tau.p))
  smat <- rbind(smat, c(x$n0, x$n0.ci, x$n0.p))
  # round to significant digits
  smat <- signif(smat, sigfigs)
  # convert to data table
  medi_result_dt <- as.data.table(smat)
  # add row labels
  medi_result_dt <- data.table(
    effect = c("indirect", "direct", "total", "proportion_mediated"),
    medi_result_dt
  )
  # confidence level
  conf_lvl <- 100 * x$conf.level
  # change column names
  names(medi_result_dt) <- c(
    "effect", "estimate", paste0("ci_", conf_lvl, c("_ll", "_ul")), "p")
  # print unless silent
  if (silent == FALSE) {
    cat(paste0(
      "\nMediation analysis using 'mediation' package ",
      "(Tingley et al. 2019; v",
      utils::packageVersion("mediation"),
      ")\n\nQuasi-Bayesian Confidence Intervals\n\n"
    ))
    print(medi_result_dt)
    cat("\nSample Size Used:", x$nobs, "\n\n")
    cat("Simulations:", x$sims, "\n")
  }
  if (output_type == "mediate_output") {
    invisible(x)
  } else if (output_type == "summary_dt") {
    invisible(medi_result_dt)
  } else if (output_type == "indirect_effect_p") {
    invisible(medi_result_dt[get("effect") == "indirect", get("p")])
  }
}
