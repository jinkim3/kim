#' Contingency table
#'
#' Create a contingency table that takes two variables as inputs
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
#' @param output_type If \code{output_type == "dt"} the output will be a
#' contingency table as a data.table object.
#' If \code{output_type == "table"} the output will be a contingency table
#' as a table object.
#' If \code{output_type == "df"} the output will be a contingency
#' table as a data.frame object.
#' By default, \code{output_type == "table"}.
#' @examples
#' contingency_table(
#' data = mtcars,
#' row_var_name = "am",
#' col_var_name = "cyl")
#' contingency_table(row = mtcars$cyl, col = mtcars$am)
#' contingency_table(mtcars, "am", "cyl", output_type = "dt")
#' @export
contingency_table <- function(
  data = NULL,
  row_var_name = NULL,
  col_var_name = NULL,
  row = NULL,
  col = NULL,
  output_type = "table"
) {
  # check inputs ----
  if (!is.null(data) & !is.null(row_var_name) & !is.null(col_var_name)) {
    # ct is short for contingency table ----
    ct <- table(data[[row_var_name]], data[[col_var_name]])
  } else if (!is.null(row) & !is.null(col)) {
    ct <- table(row, col)
    names(dimnames(ct)) <- NULL
  } else {
    stop(paste0(
      "Please check your inputs. You must specify either set of ",
      "inputs below.\n",
      "Set 1: 'data', 'row_var_name', and 'col_var_name'\n",
      "Set 2: 'row' and 'col' (i.e., two vectors, one for row and",
      " another for column)"))
  }
  if (output_type == "table") {
    output <- ct
  } else if (output_type == "dt") {
    output <- data.table::data.table(
      " " = dimnames(ct)[[1]],
      data.table::setDT(as.data.frame.matrix(ct)))
  } else if (output_type == "df") {
    output <- as.data.frame.matrix(ct)
  }
  return(output)
}
