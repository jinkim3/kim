#' Levene's test
#'
#' Conduct Levene's test (i.e., test the null hypothesis that the variances
#' in different gorups are equal)
#'
#' @param data a data object (a data frame or a data.table)
#' @param dv_name name of the dependent variable
#' @param iv_1_name name of the first independent variable
#' @param iv_2_name name of the second independent variable
#' @param round_p number of decimal places to which to round the
#' p-value from Levene's test (default = 3)
#' @param round_f number of decimal places to which to round the
#' F-statistic from Levene's test (default = 2)
#' @param output_type If \code{output_type = "text"}, the output will be
#' the results of Levene's test in a text format (i.e., character).
#' If \code{output_type = "list"}, the output will be the results of
#' Levene's test in a list format (e.g., p value, F stat, etc. as a list).
#' By default, \code{output_type = "text"}
#' @return the output of the function depends on the input for
#' \code{output_type}. By default, the output will be the
#' results of Levene's test in a text format (i.e., character).
#' @examples
#' \dontrun{
#' levene_test(
#' data = mtcars, dv_name = "mpg",
#' iv_1_name = "vs", iv_2_name = "am")
#' }
#' @export
#' @import data.table
levene_test <- function(
    data = NULL,
    dv_name = NULL,
    iv_1_name = NULL,
    iv_2_name = NULL,
    round_f = 2,
    round_p = 3,
    output_type = "text") {
  # installed packages
  installed_pkgs <- rownames(utils::installed.packages())
  # check if Package 'car' is installed
  if (!"car" %in% installed_pkgs) {
    message(paste0(
      "To conduct a two-way ANOVA, Package 'car' must ",
      "be installed.\nTo install Package 'car', type ",
      "'kim::prep(car)'",
      "\n\nAlternatively, to install all packages (dependencies) required ",
      "for all\nfunctions in Package 'kim', type ",
      "'kim::install_all_dependencies()'"))
    return()
  } else {
    # proceed if Package 'car' is already installed
    levene_test_fn_from_car <- utils::getFromNamespace(
      "leveneTest", "car")
    anova_fn_from_car <- utils::getFromNamespace(
      "Anova", "car")
  }
  # convert data to data table
  dt1 <- data.table::setDT(data.table::copy(data))
  dt1 <- dt1[, c(iv_1_name, iv_2_name, dv_name), with = FALSE]
  # convert iv to factors
  for (col in c(iv_1_name, iv_2_name)) {
    data.table::set(dt1, j = col, value = as.factor(dt1[[col]]))
  }
  # run the test
  formula_1 <- stats::as.formula(
    paste0(dv_name, " ~ ", iv_1_name, " * ", iv_2_name))
  levene_test_result <- levene_test_fn_from_car(
    y = formula_1, data = dt1)
  df <- paste0(levene_test_result[["Df"]], collapse = ", ")
  p <- levene_test_result[["Pr(>F)"]][1]
  f <- levene_test_result[["F value"]][1]
  conclusion_middle_part <- paste0(
    "F(", df, ") = ", format(round(f, round_f), nsmall = round_f),
    ", ", kim::pretty_round_p_value(
      p, round_digits_after_decimal = round_p, include_p_equals = TRUE))
  if (p < 0.05) {
    conclusion <- paste0(
      "The variances were different for the groups, ",
      conclusion_middle_part,
      ".\nThe homogeneity of variance assumption WAS VIOLATED.")
  } else if (p >= 0.05) {
    conclusion <- paste0(
      "The variances were similar for the groups, ",
      conclusion_middle_part,
      ".\nThe homogeneity of variance assumption was NOT violated.")
  }
  # output
  if (output_type == "text") {
    message(conclusion)
    invisible(conclusion)
  } else if (output_type == "list") {
    output <- list(
      test_conclusion = conclusion,
      p = p,
      df = df,
      f = f)
    return(output)
  }
}
