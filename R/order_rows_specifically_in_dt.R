#' Order rows specifically in a data table
#'
#' Order rows in a data.table in a specific order
#'
#' @param dt a data.table object
#' @param col_to_order_by a character value indicating
#' the name of the column by which to order the data.table
#' @param specific_order a vector indicating a specific order of
#' the values in the column by which to order the data.table.
#' @return a data.table object whose rows will be ordered as specified
#' @examples
#' order_rows_specifically_in_dt(mtcars, "carb", c(3, 2, 1, 4, 8, 6))
#' @import data.table
#' @export
order_rows_specifically_in_dt <- function(
  dt = NULL,
  col_to_order_by = NULL,
  specific_order = NULL) {
  # coerce into a data table and create a copy
  dt2 <- copy(setDT(dt))
  # original order
  old_order <- rep(dt2[[col_to_order_by]])
  # unique values in the original order, sorted
  old_order_unique_values <- sort(unique(old_order))
  # unique values in the new (specific) order, sorted
  specific_order_unique_values <- sort(unique(specific_order))
  # ensure that the unique values in the original vector are identical
  # to the unique values in the vector indicating the new order
  if (!identical(
    old_order_unique_values, specific_order_unique_values)) {
    # length of the longer vector
    max_length <- max(
      length(old_order_unique_values),
      length(specific_order_unique_values))
    # print the unique values
    unique_values_dt <- data.table(
      old_order_unique_values =
        old_order_unique_values[seq_len(max_length)],
      new_order_unique_values =
        specific_order_unique_values[seq_len(max_length)])
    print(unique_values_dt)
    stop(paste0(
      "The unique values in the specific (new) order are not identical",
      " to the unique values in the column to order by."))
  }
  # if length of the vector indicating the new order is shorter
  # than length of the original vector, repeat values
  if (length(specific_order) < length(old_order)) {
    new_order <- old_order[order(match(old_order, specific_order))]
  } else if (length(specific_order) > length(old_order)) {
    stop(paste0(
      "Length of the specific (new) order vector is greater than length",
      " of the column to order by."))
  } else {
    new_order <- specific_order
  }
  # reorder
  output <- dt2[match(
    make.unique(as.character(new_order)),
    make.unique(as.character(old_order)))]
  return(output)
}
