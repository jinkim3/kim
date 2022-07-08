#' Loglinear analysis
#'
#' Conduct a loglinear analysis
#'
#' @param data a data object (a data frame or a data.table)
#' @param dv_name name of the dependent variable
#' @param iv_1_name name of the first independent variable
#' @param iv_2_name name of the second independent variable
#' @param iv_1_values restrict all analyses to observations having
#' these values for the first independent variable
#' @param iv_2_values restrict all analyses to observations having
#' these values for the second independent variable
#' @param output type of the output. If \code{output_type = "all"},
#' the function will return a results summary and print a mosaic plot.
#' (default = "all")
#' @param round_p number of decimal places to which to round
#' p-values (default = 3)
#' @param round_chi_sq number of decimal places to which to round
#' chi-squared test statistics (default = 2)
#' @param mosaic_plot If \code{mosaic_plot = TRUE}, a mosaic plot will
#' be printed (default = TRUE)
#' @param report_as_field If \code{report_as_field = TRUE}, reports summary
#' will follow the format suggested by Andy Field (2012)
#' (ISBN: 978-1-4462-0045-2, p. 851)
#' @examples
#' \donttest{
#' loglinear_analysis(data = data.frame(Titanic), "Survived", "Sex", "Age")
#' }
#' @export
loglinear_analysis <- function(
  data = NULL,
  dv_name = NULL,
  iv_1_name = NULL,
  iv_2_name = NULL,
  iv_1_values = NULL,
  iv_2_values = NULL,
  output = "all",
  round_p = 3,
  round_chi_sq = 2,
  mosaic_plot = TRUE,
  report_as_field = FALSE) {
  # bind the vars locally to the function
  round_trail_0 <- NULL
  # installed packages
  installed_pkgs <- rownames(utils::installed.packages())
  # check if Package 'MASS' is installed ----
  if (!"MASS" %in% installed_pkgs) {
    message(paste0(
      "This function requires the installation of Package 'MASS'.",
      "\nTo install Package 'MASS', type ",
      "'kim::prep(MASS)'",
      "\n\nAlternatively, to install all packages (dependencies) required ",
      "for all\nfunctions in Package 'kim', type ",
      "'kim::install_all_dependencies()'"))
    return()
  } else {
    # proceed if Package 'MASS' is already installed
    loglm_from_MASS <- utils::getFromNamespace(
      "loglm", "MASS")
  }
  # check inputs ----
  if (is.null(data)) {
    stop("Please specify a data set for the analysis.")
  }
  if (is.null(dv_name)) {
    stop("The input dv_name is missing.")
  }
  if (is.null(iv_1_name)) {
    stop("The input iv_1_name is missing.")
  }
  if (is.null(iv_2_name)) {
    stop("The input iv_2_name is missing.")
  }
  # convert data to data table ----
  dt1 <- data.table::setDT(data.table::copy(data))
  dt1 <- dt1[, c(iv_1_name, iv_2_name, dv_name), with = FALSE]
  # convert iv to factors
  for (col in c(iv_1_name, iv_2_name)) {
    data.table::set(dt1, j = col, value = as.factor(dt1[[col]]))
  }
  # remove na
  dt2 <- stats::na.omit(dt1)
  # count na rows
  if (nrow(dt1) != nrow(dt2)) {
    kim::pm(
      nrow(dt1) - nrow(dt2),
      " rows were removed due to missing values.")
  }
  # subset for certain values
  if (!is.null(iv_1_values)) {
    dt2 <- dt2[get(iv_1_name) %in% iv_1_values]
  }
  if (!is.null(iv_2_values)) {
    dt2 <- dt2[get(iv_2_name) %in% iv_2_values]
  }
  # contingency table ----
  # ct stands for contingency table ----
  formula_1 <- stats::as.formula(
    paste0("~ ", iv_1_name, " + ", iv_2_name, " + ", dv_name))
  ct_1 <- stats::xtabs(formula = formula_1, data = dt2)
  # saturated model ----
  formula_2 <- stats::as.formula(
    paste0("~ ", iv_1_name, " * ", iv_2_name, " * ", dv_name))
  saturated <- loglm_from_MASS(formula = formula_2, data = ct_1)
  # summary(saturated)
  # three way interaction ----
  formula_3 <- stats::as.formula(
    paste0("~ ", iv_1_name, " + ", iv_2_name, " + ", dv_name, " + ",
           iv_1_name, ":", iv_2_name, " + ",
           iv_1_name, ":", dv_name, " + ",
           iv_2_name, ":", dv_name))
  three_way <- loglm_from_MASS(formula = formula_3, data = ct_1)
  # summary(three_way)
  # mc stands model comparison ----
  mc_1 <- stats::anova(saturated, three_way)
  # interaction p value ----
  # int stands for interaction ----
  int_p <- mc_1["Model 2", "P(> Delta(Dev)"]
  # chi squared test statistic ----
  chi_sq_test_stat <- mc_1["Model 1", "Deviance"]
  chi_sq_df <- mc_1["Model 1", "df"]
  # report ----
  # rss stands for results summary section ----
  rss_1 <- "A three-way loglinear analysis "
  rss_2 <- ifelse(
    report_as_field == TRUE, paste0(
      "produced a final model that retained all effects. The likelihood ",
      "ratio of this model was chi-squared (0) = 0, p = 1. This ",
      "indicated "),
    "revealed ")
  rss_3 <- paste0(
    "that the highest order interaction (",
    iv_1_name, " x ", iv_2_name, " x ", dv_name, ") was ")
  rss_4 <- ifelse(
    int_p < 0.05, "significant", "not significant")
  rss_5 <- paste0(
    ", chi-squared (", chi_sq_df, ") = ",
    kim::und(
      round_trail_0, x = chi_sq_test_stat,
      digits = round_chi_sq), ", ",
    kim::pretty_round_p_value(
      int_p,
      round_digits_after_decimal = round_p,
      include_p_equals = TRUE))
  rs <- paste0(rss_1, rss_2, rss_3, rss_4, rss_5)
  # return results summary ----
  if (output == "results_summary") {
    return(rs)
  }
  message(rs)
  # mosaic plot ----
  if (mosaic_plot == TRUE) {
    graphics::mosaicplot(ct_1, shad = TRUE, main = paste0(
      dv_name, " = ", iv_1_name, " x ", iv_2_name))
  }
}
