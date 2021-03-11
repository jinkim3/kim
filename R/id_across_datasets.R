#' ID across datasets
#'
#' Create an ID column in each of the data sets. The ID values will
#' span across the data sets.
#'
#' @param dt_list a list of data.table objects
#' @param id_col_name name of the column that will contain ID values
#' @param id_col_position position of the newly created ID column.
#' If \code{id_col_position = "first"}, the new ID column will
#' be placed as the first column in respective data sets.
#' If \code{id_col_position = "last"}, the new ID column will
#' be placed as the last column in respective data sets.
#' @examples
#' id_across_datasets(
#' dt_list = list(setDT(copy(mtcars)), setDT(copy(iris))))
#' id_across_datasets(
#' dt_list = list(setDT(copy(mtcars)), setDT(copy(iris)), setDT(copy(women))),
#' id_col_name = "newly_created_id_col",
#' id_col_position = "last")
#' @export
id_across_datasets <- function(
  dt_list = NULL,
  id_col_name = "id",
  id_col_position = "first"
) {
  # check the dt_list argument
  if (is.list(dt_list) == FALSE) {
    stop('The input for the argument "dt_list" is not a list.')
  }
  if (data.table::is.data.table(dt_list[[1]]) == FALSE) {
    stop('The first element of the argument "dt_list" is not a data.table.')
  }
  # check if id_col_name already exists in any data sets
  for (i in seq_along(dt_list)) {
    if (id_col_name %in% names(dt_list[[i]])) {
      stop(paste0(
        'A column with the name "', id_col_name, '" already exists in ',
        "dt_list[[", i, "]]."))
    }
  }
  # starting id values
  starting_id_values <- ending_id_values <- rep(NA, length(dt_list))
  # add the id column
  for (i in seq_along(dt_list)) {
    # starting id for the first data set
    if (i == 1) {
      starting_id <- 1
    }
    # give id values in each data set
    ending_id <- starting_id + nrow(dt_list[[i]]) - 1
    dt_list[[i]][[id_col_name]] <- starting_id:ending_id
    # record starting and ending id values to print out later
    starting_id_values[i] <- starting_id
    ending_id_values[i] <- ending_id
    # new starting id for the data set in the next iteration
    starting_id <- ending_id + 1
    # move id column to the beginning
    if (id_col_position == "first") {
      setcolorder(dt_list[[i]], id_col_name)
    }
  }
  # report success
  message(paste0(
    'The ID column with the name "', id_col_name,
    '" was created in each of the ', length(dt_list), " data sets."))
  # print summary
  summary_dt <- data.table(
    data_set = seq_along(dt_list),
    start_id = starting_id_values,
    end_id = ending_id_values)
  print(summary_dt)
  # return
  output <- dt_list
  invisible(output)
}
