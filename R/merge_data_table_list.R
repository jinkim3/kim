#' Merge a list of data tables
#'
#' Successively merge a list of data.table objects in a recursive
#' fashion. That is, merge the (second data table in the list) around
#' the first data table in the list; then, around this resulting data table,
#' merge the third data table in the list; and so on.
#'
#' If there are any duplicated ID values and column names across
#' the data tables, the cell values in the earlier data table will
#' remain intact and the cell values in the later data table will be
#' discarded for the resulting merged data table in each recursion.
#'
#' @param dt_list a list of data.table objects
#' @param id name of the column that will contain the ID values
#' in the data tables. The name of the ID column must be identical
#' in the all data tables.
#' @param silent If \code{silent = TRUE}, no message will be printed
#' regarding how many ID values and column names were duplicated.
#' If \code{silent = FALSE}, messages will be printed regarding
#' how many ID values and column names were duplicated. (default = FALSE)
#' @return a data.table object, which successively merges (joins)
#' a data table around (i.e., outside) the previous data table in the
#' list of data tables.
#' @examples
#' data_1 <- data.table::data.table(
#' id_col = c(4, 2, 1, 3),
#' a = 3:6,
#' b = 5:8,
#' c = c("w", "x", "y", "z"))
#' data_2 <- data.table::data.table(
#' id_col = c(1, 4, 99),
#' d = 6:8,
#' b = c("p", "q", "r"),
#' e = c(TRUE, FALSE, FALSE))
#' data_3 <- data.table::data.table(
#' id_col = c(200, 3),
#' f = 11:12,
#' b = c(300, "abc"))
#' merge_data_table_list(
#' dt_list = list(data_1, data_2, data_3), id = "id_col")
#' @import data.table
#' @export
merge_data_table_list <- function(
  dt_list = NULL,
  id = NULL,
  silent = TRUE
) {
  # recursively apply the merge_data_tables function
  output <- Reduce(function(x, y) {
    kim::merge_data_tables(x, y, id = id, silent = silent)}, dt_list)
  return(output)
}
