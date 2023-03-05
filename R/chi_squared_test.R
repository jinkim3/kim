#' Chi-squared test
#'
#' Conduct a chi-squared test and produce a contingency table
#'
#' @param data a data object (a data frame or a data.table)
#' @param row_var_name name of the variable whose values will fill the
#' rows of the contingency table
#' @param col_var_name name of the variable whose values will fill the
#' columns of the contingency table
#' @param row a vector whose values will fill the rows of the
#' contingency table
#' @param col a vector whose values will fill the columns of the
#' contingency table
#' @param sigfigs number of significant digits to round to (for the
#' frequency table). By default \code{sigfigs = 2}
#' @param correct logical. Should continuity correction be applied?
#' (default = TRUE)
#' @examples
#' chi_squared_test(data = mtcars, row_var_name = "am", col_var_name = "cyl")
#' chi_squared_test(row = mtcars$cyl, col = mtcars$am)
#' chi_squared_test(mtcars, "am", "cyl", output_type = "dt")
#' @export
chi_squared_test <- function(
  data = NULL,
  row_var_name = NULL,
  col_var_name = NULL,
  row = NULL,
  col = NULL,
  sigfigs = 2,
  correct = TRUE
) {
  # check inputs
  if (!is.null(data) & !is.null(row_var_name) & !is.null(col_var_name)) {
    # ct is short for contingency table ----
    ct <- table(data[[row_var_name]], data[[col_var_name]])
    # chi-squared test
    chi_sq_test_results <- stats::chisq.test(
      data[[row_var_name]], data[[col_var_name]], correct = correct)
  } else if (!is.null(row) & !is.null(col)) {
    ct <- table(row, col)
    names(dimnames(ct)) <- NULL
    # chi-squared test
    chi_sq_test_results <- stats::chisq.test(row, col, correct = correct)
  } else {
    stop(paste0(
      "Please check your inputs. You must specify either set of ",
      "inputs below.\n",
      "Set 1: 'data', 'row_var_name', and 'col_var_name'\n",
      "Set 2: 'row' and 'col' (i.e., two vectors, one for row and",
      " another for column)"))
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
  if (!is.null(data) & !is.null(row_var_name) & !is.null(col_var_name)) {
    # chi-squared test
    print(stats::chisq.test(
      data[[row_var_name]], data[[col_var_name]], correct = correct))
  } else if (!is.null(row) & !is.null(col)) {
    # chi-squared test
    print(stats::chisq.test(row, col, correct = correct))
  }
}
