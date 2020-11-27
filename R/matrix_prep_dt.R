#' Prepare a two-column data.table that will be used to fill values in a matrix
#'
#' @param row_var_names a vector of variable names, each of which will be
#' header of a row in the eventual matrix
#' @param col_var_names a vector of variable names, each of which will be
#' header of a column in the eventual matrix
#' @examples
#' matrix_prep_dt(
#'   row_var_names = c("mpg", "cyl"),
#'   col_var_names = c("hp", "gear")
#' )
#' @export
# create a matrix
matrix_prep_dt <- function(
  row_var_names = NULL,
  col_var_names = NULL) {
  if (is.null(col_var_names) & !is.null(row_var_names)) {
    col_var_names <- row_var_names
  }
  if (is.null(row_var_names) & !is.null(col_var_names)) {
    row_var_names <- col_var_names
  }
  # row-column pair
  v1 <- rep(row_var_names, length(col_var_names))
  v2 <- rep(col_var_names, each = length(row_var_names))
  # dt
  output <- data.table::data.table(v1, v2)
  return(output)
}
