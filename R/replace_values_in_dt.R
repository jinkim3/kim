#' Replace values in a data table
#'
#' Replace values in a data.table
#'
#' @param data a data object (a data frame or a data.table)
#' @param old_values a vector of old values that need to be replaced
#' @param new_value a new value that will replace the old values
#' @param silent If \code{silent = FALSE}, a message will be printed regarding
#' how many values were replaced. If \code{silent = TRUE}, no message
#' will be printed regarding how many values were replaced. (default = FALSE)
#' @examples
#' replace_values_in_dt(data = mtcars, old_values = c(0, 1), new_value = 999)
#' replace_values_in_dt(data = mtcars, old_values = 21.0, new_value = 888)
#' @export
replace_values_in_dt <- function(
  data = NULL,
  old_values = NULL,
  new_value = NULL,
  silent = FALSE) {
  # copy data
  dt <- data.table::setDT(data.table::copy(data))
  # check if there are multiple new values
  if (length(new_value) > 1) {
    kim::pm(
      "The function currently accepts only one value for the ",
      'input "new_value".')
  }
  # check the old values input
  if (is.null(old_values)) {
    stop(paste0(
      "Please enter a single value or a vector of values for ",
      'the argument, "old_values".'))
  }
  # check if the old values are all na
  if (identical(unique(old_values), NA)) {
    count_of_old_values <- sum(is.na(dt))
    if (count_of_old_values > 0) {
      for (col in names(dt)) {
        data.table::set(
          dt, i = which(is.na(dt[[col]])), j = col, value = new_value)
      }
    }
  }
  # replace the single, specific old value
  if (length(old_values) == 1) {
    count_of_old_values <- sum(dt == old_values, na.rm = TRUE)
    if (count_of_old_values > 0) {
      for (col in names(dt)) {
        data.table::set(
          dt, i = which(dt[[col]] == old_values), j = col, value = new_value)
      }
    }
  }
  # replace multiple old values
  if (length(old_values) > 1) {
    count_list <- lapply(old_values, function(x) {
      sum(dt == old_values, na.rm = TRUE)
    })
    count_of_old_values <- sum(unlist(count_list))
    if (count_of_old_values > 0) {
      for (col in names(dt)) {
        data.table::set(
          dt, i = which(dt[[col]] %in% old_values), j = col,
          value = new_value)
      }
    }
  }
  # report summary
  if (silent == FALSE) {
    message("A total of ", count_of_old_values, " value(s) were replaced.")
  }
  # return the output
  return(dt)
}
