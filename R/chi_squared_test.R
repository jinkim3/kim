#' Chi-squared test
#'
#' Conduct a chi-squared test and produce a contingency table
#'
#' @param data a data object (a data frame or a data.table)
#' @param iv_name name of the independent variable
#' @param dv_name name of the dependent variable (must be a binary variable)
#' @param sigfigs number of significant digits to round to (for the
#' table of proportions). By default \code{sigfigs = 2}
#' @param correct logical. Should continuity correction be applied?
#' (default = TRUE)
#' @examples
#' chi_squared_test(data = mtcars, iv_name = "cyl", dv_name = "am")
#' @export
chi_squared_test <- function(
  data = NULL,
  iv_name = NULL,
  dv_name = NULL,
  sigfigs = 2,
  correct = TRUE
) {
  # make sure the dv has only two levels of value
  values_of_dv <- unique(data[[dv_name]])
  if (length(values_of_dv) != 2) {
    stop(paste0(
      "The DV has ", length(values_of_dv), " level(s), rather than the ",
      "expected 2 levels."))
  }
  # check inputs
  if (!is.null(data) & !is.null(iv_name) & !is.null(dv_name)) {
    # ct is short for contingency table ----
    ct <- table(data[[iv_name]], data[[dv_name]])
    # chi-squared test
    chi_sq_test_results <- stats::chisq.test(
      data[[iv_name]], data[[dv_name]], correct = correct)
  } else {
    stop(paste0(
      "Please check your inputs. You must enter an input for\n",
      "'data', 'iv_name', and 'dv_name'"))
  }
  # print the contingency table
  message("Table of Counts:")
  print(ct)
  cat("\n")
  # print the table of proportions
  # "pt" stands for proportion table
  pt <- signif(prop.table(ct, margin = 1) * 100, sigfigs)
  pt2 <- data.frame(matrix(paste0(pt, "%"), nrow = nrow(pt)))
  row.names(pt2) <- dimnames(pt)[[1]]
  names(pt2) <- dimnames(pt)[[2]]
  message("Table of Proportions:")
  print(pt2)
  # chi squared test
  if (!is.null(data) & !is.null(iv_name) & !is.null(dv_name)) {
    # chi-squared test
    print(stats::chisq.test(
      data[[iv_name]], data[[dv_name]], correct = correct))
  } else if (!is.null(row) & !is.null(col)) {
    # chi-squared test
    print(stats::chisq.test(row, col, correct = correct))
  }
}
