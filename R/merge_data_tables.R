#' Merge data tables
#'
#' Merge two data.table objects
#'
#' @param dt1 the first data.table which will remain intact
#' @param dt2 the second data.table which will be joined outside of
#' (around) the first data.table. If there are any duplicated
#' ID values and column names across the two data tables, the
#' cell values in the first data.table will remain intact and
#' the cell values in the second data.table will be discarded for the
#' resulting merged data table.
#' @param id name of the column that will contain the ID values
#' in the two data tables. The name of the ID column must be identical
#' in the two data tables.
#' @param silent If \code{silent = TRUE}, no message will be printed
#' regarding how many ID values and column names were duplicated.
#' If \code{silent = FALSE}, messages will be printed regarding
#' how many ID values and column names were duplicated. (default = FALSE)
#' @return a data.table object, which merges (joins) the second data.table
#' around the first data.table.
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
#' merge_data_tables(dt1 = data_1, dt2 = data_2, id = "id_col")
#' @import data.table
#' @export
merge_data_tables <- function(
  dt1 = NULL,
  dt2 = NULL,
  id = NULL,
  silent = TRUE
) {
  # stop if an input is missing
  if (is.null(dt1)) {
    stop("Please enter an input for the first data table argument, dt1.")
  }
  if (is.null(dt2)) {
    stop("Please enter an input for the second data table argument, dt2.")
  }
  if (is.null(id)) {
    stop(paste0("Please enter an input for the ID column name, ",
                'e.g., id = "subject_id".'))
  }
  # coerce inputs into data tables if they are not already
  if (!is.data.table(dt1)) {
    dt1 <- setDT(dt1)
  }
  if (!is.data.table(dt2)) {
    dt2 <- setDT(dt2)
  }
  # check if the id column is in both data tables
  if (!id %in% names(dt1)) {
    stop(paste0('The id column "', id, '" is not found in the first',
                " data table, dt1."))
  }
  if (!id %in% names(dt2)) {
    stop(paste0('The id column "', id, '" is not found in the second',
                " data table, dt2."))
  }
  # id values in each data table; replicate so that the order is preserved
  dt1_id_values <- rep(dt1[[id]])
  dt2_id_values <- rep(dt2[[id]])
  # id values in the final dt
  id_in_final_dt <- union(dt1_id_values, dt2_id_values)
  # id values unique in dt2
  unique_id_in_dt2 <- setdiff(dt2_id_values, dt1_id_values)
  # print duplicated id values
  duplicated_id_values <- intersect(dt1_id_values, dt2_id_values)
  if (silent == FALSE) {
    message(paste0("Number of duplicated ID values: ",
                   length(duplicated_id_values)))
  }
  # column names in each data table
  dt1_col_names <- names(dt1)
  dt2_col_names <- names(dt2)
  # column names in the final dt
  col_names_in_final_dt <- union(dt1_col_names, dt2_col_names)
  # names of non-ID columns that are in both data tables
  duplicated_col_names <- setdiff(
    intersect(dt1_col_names, dt2_col_names), id)
  # print duplicated column names
  if (silent == FALSE) {
    message(paste0("Number of duplicated column names: ",
                   length(duplicated_col_names)))
  }
  # set keys in each dt
  setkeyv(dt1, id)
  setkeyv(dt2, id)
  # merge data tables
  merged_dt <- merge(dt1, dt2, all = TRUE)
  # merge duplicated columns
  merged_cols <- lapply(duplicated_col_names, function(x) {
    dt1_rows <- merged_dt[[id]] %in% dt1_id_values
    v1 <- merged_dt[[paste0(x, ".x")]][dt1_rows]
    dt2_rows <- merged_dt[[id]] %in% unique_id_in_dt2
    v2 <- merged_dt[[paste0(x, ".y")]][dt2_rows]
    output <- c(v1, v2)
    return(output)
  })
  # give names for merged_cols
  names(merged_cols) <- duplicated_col_names
  # replace the first set of duplicated columns (those with suffix ".x")
  # with the newly created merged columns
  cols_to_replace <- paste0(duplicated_col_names, ".x")
  for (col in cols_to_replace) {
    set(merged_dt, j = col,
        value = merged_cols[[gsub("\\.x$", "", col)]])
  }
  # remove the second set of duplicated columns (those with suffix ".y")
  cols_to_remove <- paste0(duplicated_col_names, ".y")
  merged_dt[, (cols_to_remove) := NULL]
  setnames(merged_dt, old = cols_to_replace, duplicated_col_names)
  # restore the original order of rows
  output <- kim::order_rows_specifically_in_dt(
    dt = merged_dt,
    col_to_order_by = id,
    specific_order = id_in_final_dt)
  # output
  return(output)
}
