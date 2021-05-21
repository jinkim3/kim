#' Contingency Table
#'
#' Create a contingency table.
#'
#' @param data a data object (a data frame or a data.table)
#' @param dv_name name of the binary dependent variable
#' @param iv_names a character vector containing names of the
#' independent variables
#' @param list_of_levels_in_ivs a list of character vectors, each of
#' which will be used to subset the data and determine the order of
#' levels in the respective independent variables
#' @param sigfigs number of significant digits to which to round
#' values in the contingency table (default = 2)
#' @return the output will be a contingency table in a data.table format
#' @examples
#' contingency_table(
#'   data = mtcars, dv_name = "am", iv_names = c("cyl", "vs"))
#' @export
#' @import data.table
contingency_table <- function(
  data = NULL,
  dv_name = NULL,
  iv_names = NULL,
  list_of_levels_in_ivs = NULL,
  sigfigs = 3) {
  # copy data
  dt <- data.table::setDT(data.table::copy(data))
  # check whether the variables are in the data set
  all_vars <- c(dv_name, iv_names, "a", "b")
  missing_vars <- all_vars[which(!(all_vars %in% names(dt)))]
  # stop if any variables are missing in the data set
  if (length(missing_vars) > 0) {
    stop(paste0(
      "The following variables were not found in the data: ",
      paste0(missing_vars, collapse = ", ")))
  }

  return()
}
