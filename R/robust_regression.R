#' Robust regression (bootstrapped regression)
#'
#' Estimate coefficients in a multiple regression model by bootstrapping.
#'
#' The following package(s) must be installed prior to running this function:
#' Package 'boot' v1.3-26 (or possibly a higher version) by
#' Canty & Ripley (2021),
#' <https://cran.r-project.org/package=boot>
#'
#' @param data a data object (a data frame or a data.table)
#' @param formula a formula object for the regression equation
#' @param sigfigs number of significant digits to round to
#' @param round_digits_after_decimal
#' round to nth digit after decimal
#' (alternative to \code{sigfigs})
#' @param iterations number of bootstrap samples. The default is set at 1000,
#' but consider increasing the number of samples to 5000, 10000, or an
#' even larger number, if slower handling time is not an issue.
#' @examples
#' \dontrun{
#' robust_regression(
#'   data = mtcars, formula = mpg ~ cyl * hp,
#'   iterations = 100
#' )
#' }
#' @export
robust_regression <- function(
  data = NULL, formula = NULL, sigfigs = NULL,
  round_digits_after_decimal = NULL,
  iterations = 1000) {
  # check if Package 'boot' is installed
  if (!"boot" %in% rownames(utils::installed.packages())) {
    message(paste0(
      "To conduct robust regression, Package 'boot' must ",
      "be installed.\nTo install Package 'boot', type ",
      "'kim::prep(boot)'",
      "\n\nAlternatively, to install all packages (dependencies) required ",
      "for all\nfunctions in Package 'kim', type ",
      "'kim::install_all_dependencies()'"))
    return()
  } else {
    # proceed if Package 'boot' is already installed
    boot_fn_from_boot <- utils::getFromNamespace(
      "boot", "boot")
    boot_ci_fn_from_boot <- utils::getFromNamespace(
      "boot.ci", "boot")
  }
  # print lm first
  lm_table <- kim::multiple_regression(
    data = data, formula = formula, sigfigs = sigfigs,
    round_digits_after_decimal = round_digits_after_decimal
  )
  # function to obtain regression weights
  bs <- function(formula = formula, data = data, indices) {
    bs_sample <- data[indices, ] # allows boot to select sample
    fit <- stats::lm(formula = formula, data = bs_sample)
    return(stats::coef(fit))
  }
  # adjust iterations to be larger than the number of data rows
  iterations <- max(iterations, nrow(data))
  # print progress
  message("Getting bootstrap confidence intervals...")
  # bootstrapping with 2000+ replications
  boot_results <- boot_fn_from_boot(
    data = data,
    statistic = bs,
    R = iterations, formula = formula, parallel = "snow"
  )
  # formula
  terms <- names(boot_results[["t0"]])
  ci_95 <- lapply(seq_along(terms), function(i) {
    temp_1 <- boot_ci_fn_from_boot(boot_results, type = "bca", index = i)
    return(utils::tail(temp_1[["bca"]][1, ], 2))
  })
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
    ci_95 <- lapply(ci_95, signif, sigfigs)
  }
  if (!is.null(round_digits_after_decimal)) {
    ci_95 <- lapply(ci_95, round, round_digits_after_decimal)
  }
  robust_estimate_95_ci <- as.list(c(
    vapply(ci_95, paste0,
           FUN.VALUE = character(1L),
           collapse = ", "
    ), rep("", 7)
  ))
  t1 <- data.table::data.table(lm_table, robust_estimate_95_ci)
  data.table::setcolorder(t1, c(
    "variable", "estimate", "se", "robust_estimate_95_ci"
  ))
  return(t1)
}
