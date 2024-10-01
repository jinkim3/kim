#' Chi-squared test
#'
#' Conduct a chi-squared test and produce a contingency table
#'
#' @param data a data object (a data frame or a data.table)
#' @param iv_name name of the independent variable
#' @param dv_name name of the dependent variable (must be a binary variable)
#' @param round_chi_sq_test_stat number of decimal places to which to
#' round the chi-squared test statistic (default = 2)
#' @param round_p number of decimal places to which to round the
#' p-value from the chi-squared test (default = 3)
#' @param sigfigs_proportion number of significant digits to round to
#' (for the table of proportions). By default \code{sigfigs_proportion = 2}
#' @param correct logical. Should continuity correction be applied?
#' (default = TRUE)
#' @param odds_ratio_ci width of the confidence interval for the odds ratio.
#' Input can be any value less than 1 and greater than or equal to 0.
#' By default, \code{odds_ratio_ci = 0.95}.
#' If \code{odds_ratio_ci = TRUE}, the default value of 0.95 will be used.
#' If \code{odds_ratio_ci = FALSE}, no confidence interval will be estimated
#' for the odds ratio.
#' @param round_odds_ratio_ci_limits number of decimal places to which to
#' round the limits of the odds ratio's confidence interval (default = 2)
#' @param invert logical. Whether the inverse of the odds ratio
#' (i.e., 1 / odds ratio) should be returned.
#' @param notify_na_count if \code{TRUE}, notify how many rows
#' were removed due to missing values. By default, NA count will be printed
#' only if there are any NA values.
#' @examples
#' chi_squared_test(data = mtcars, iv_name = "cyl", dv_name = "am")
#' # if the iv has only two levels, odds ratio will also be calculated
#' chi_squared_test(data = mtcars, iv_name = "vs", dv_name = "am")
#' @export
chi_squared_test <- function(
  data = NULL,
  iv_name = NULL,
  dv_name = NULL,
  round_chi_sq_test_stat = 2,
  round_p = 3,
  sigfigs_proportion = 2,
  correct = TRUE,
  odds_ratio_ci = 0.95,
  round_odds_ratio_ci_limits = 2,
  invert = FALSE,
  notify_na_count = NULL
) {
  # check inputs
  if (is.null(data) | is.null(iv_name) | is.null(dv_name)) {
    stop(paste0(
      "Please check your inputs. You must enter an input for\n",
      "'data', 'iv_name', and 'dv_name'"))
  }
  # convert data to data table
  dt1 <- data.table::setDT(data.table::copy(data))
  # deal with NA values
  dt2 <- stats::na.omit(dt1[, c(iv_name, dv_name), with = FALSE])
  na_count <- nrow(dt1) - nrow(dt2)
  # by default, notify only if NA values are present
  if (is.null(notify_na_count)) {
    notify_na_count <- ifelse(na_count > 0, TRUE, FALSE)
  }
  if (notify_na_count == TRUE) {
    message(paste0(
      "\n", na_count,
      " rows(s) were excluded from analysis due to missing values.\n"
    ))
  }
  # make sure the dv has only two levels of value
  values_of_dv <- unique(dt2[[dv_name]])
  if (length(values_of_dv) < 2) {
    stop(paste0(
      "The DV has ", length(values_of_dv), " level(s), rather than the ",
      "expected 2 levels (or more)."))
  } else if (length(values_of_dv) > 2) {
    stop(paste0(
      "The current version of this function can only handle a DV with",
      " 2 levels.\n",
      "For a DV with more than two levels, try the 'CrossTable'",
      "function\nin the 'gmodels' package as in the following example:\n",
      "gmodels::CrossTable(mtcars$cyl, mtcars$carb)"))
  }
  # ct is short for contingency table
  ct <- table(dt2[[iv_name]], dt2[[dv_name]])
  ct2 <- data.frame(matrix(ct, nrow(ct)))
  row.names(ct2) <- dimnames(ct)[[1]]
  names(ct2) <- dimnames(ct)[[2]]
  # print the contingency table
  message("Table of Counts:")
  print(ct2)
  cat("\n")
  # print the table of proportions
  # "pt" stands for proportion table
  pt <- signif(prop.table(ct, margin = 1) * 100, sigfigs_proportion)
  pt2 <- data.frame(matrix(paste0(pt, "%"), nrow = nrow(pt)))
  row.names(pt2) <- dimnames(pt)[[1]]
  names(pt2) <- dimnames(pt)[[2]]
  message("Table of Proportions:")
  print(pt2)
  # chi squared test
  chi_sq_test_results <- stats::chisq.test(
    dt2[[iv_name]], dt2[[dv_name]], correct = correct)
  chi_sq_test_df <- chi_sq_test_results[["parameter"]][["df"]]
  chi_sq_test_stat <- chi_sq_test_results[["statistic"]][["X-squared"]]
  chi_sq_test_p <- chi_sq_test_results[["p.value"]]
  chi_sq_test_inference_pt_1 <- "The proportions (counts) associated with '"
  chi_sq_test_inference_pt_2 <- paste0(dv_name, "'\n")
  chi_sq_test_inference_pt_3 <- data.table::fcase(
    chi_sq_test_p > 0.1,
    "did not vary as a function of ",
    chi_sq_test_p >= 0.05,
    "varied marginally significantly as a function of ",
    chi_sq_test_p < 0.05,
    "varied significantly as a function of ")
  chi_sq_test_inference_pt_4 <- paste0("'", iv_name, "',\n")
  chi_sq_test_inference_pt_5 <- paste0(
    "Chi-squared (df = ", chi_sq_test_df, ") = ",
    round(chi_sq_test_stat, round_chi_sq_test_stat),
    ", ", kim::pretty_round_p_value(
      chi_sq_test_p, round_p, include_p_equals = TRUE))
  cat("\n")
  message(paste0(
    chi_sq_test_inference_pt_1,
    chi_sq_test_inference_pt_2,
    chi_sq_test_inference_pt_3,
    chi_sq_test_inference_pt_4,
    chi_sq_test_inference_pt_5))
  cat("\n")
  # calculate the odds ratio if the iv has two levels
  values_of_iv <- unique(dt2[[iv_name]])
  if (length(values_of_iv) == 2) {
    kim::odds_ratio(
      data = dt2, iv_name = iv_name, dv_name = dv_name,
      ci = odds_ratio_ci,
      round_ci_limits = round_odds_ratio_ci_limits,
      invert = invert)
  }
}
