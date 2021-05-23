#' Pivot Table
#'
#' Create a pivot table.
#'
#' @param data a data object (a data frame or a data.table)
#' @param row_names names of variables for constructing rows
#' @param col_names names of variables for constructing columns
#' independent variables
#' @param function_as_character function to perform for each cell in
#' the pivot table
#' @param sigfigs number of significant digits to which to round
#' values in the pivot table (default = 3)
#' @param output type of output. If \code{output = "dt"}, the function's
#' output will be a pivot table in a data.table format.
#' If \code{output = "subsets"}, the function's output will be a list of
#' data tables that are subsets representing each cell in the pivot table.
#' By default, \code{output = "dt"}
#' @param remove_col_names logical. Should the column names
#' (i.e., v1, v2, ...) be removed in the data table output?
#' @return the output will be a contingency table in a data.table format
#' @examples
#' pivot_table(
#'   data = mtcars, col_names = "am", row_names = c("cyl", "vs"),
#'   function_as_character = "mean(mpg)")
#' pivot_table(
#'   data = mtcars, col_names = "am", row_names = c("cyl", "vs"),
#'   function_as_character = "sum(mpg < 17)")
#' pivot_table(
#'   data = mtcars, col_names = "am", row_names = c("cyl", "vs"),
#'   function_as_character =
#'   "round(sum(mpg < 17) / sum(!is.na(mpg)) * 100, 0)")
#' @export
#' @import data.table
pivot_table <- function(
  data = NULL,
  row_names = NULL,
  col_names = NULL,
  function_as_character = NULL,
  sigfigs = 3,
  output = "dt",
  remove_col_names = TRUE) {
  # copy data
  dt <- data.table::setDT(data.table::copy(data))
  # check whether the variables are in the data set
  all_vars <- c(row_names, col_names)
  missing_vars <- all_vars[which(!(all_vars %in% names(dt)))]
  # stop if any variables are missing in the data set
  if (length(missing_vars) > 0) {
    stop(paste0(
      "The following variables were not found in the data: ",
      paste0(missing_vars, collapse = ", ")))
  }
  # levels in the row variables
  levels_in_row_vars <- lapply(seq_along(row_names), function(i) {
    return(kim::su(dt[[rev(row_names)[i]]]))
  })
  # row headers
  row_headers <- rev(expand.grid(levels_in_row_vars))
  # fix names for row headers
  names(row_headers) <- row_names
  # levels in the column variables
  levels_in_col_vars <- lapply(seq_along(col_names), function(i) {
    return(kim::su(dt[[rev(col_names)[i]]]))
  })
  # column headers
  col_headers <- rev(expand.grid(levels_in_col_vars))
  # fix names for column headers
  names(col_headers) <- col_names
  # column and row variables
  col_and_row_vars <- c(col_names, row_names)
  # levels in the row variables
  levels_in_col_and_row_vars <- lapply(
    seq_along(col_and_row_vars), function(i) {
      return(kim::su(dt[[rev(col_and_row_vars)[i]]]))
    })
  # dt for subsets
  dt_for_subsetting <- data.table::setDT(
    rev(expand.grid(levels_in_col_and_row_vars)))
  names(dt_for_subsetting) <- col_and_row_vars
  # return subset
  if (output == "subsets") {
    dt_list <- lapply(seq_len(nrow(dt_for_subsetting)), function(i) {
      # get list of values for subsetting
      value_list <- as.list(dt_for_subsetting[i, ])
      # which rows of data should be in the subset?
      rows_for_subset <- Reduce(
        `&`, Map(`==`, dt[, col_and_row_vars, with = FALSE], value_list))
      return(dt[rows_for_subset])
    })
  }
  # perform a function for each subset
  if (is.null(function_as_character)) {
    stop(paste0(
      "Please specify the function, e.g., ",
      'function_as_character = "mean(age)"'))
  }
  # notify the function to perform
  kim::pm("The following function was performed for each cell ",
     "in the data table below: ", function_as_character)
  # collect the function output from each subset
  function_output <- lapply(seq_len(nrow(dt_for_subsetting)), function(i) {
    # get list of values for subsetting
    value_list <- as.list(dt_for_subsetting[i, ])
    # which rows of data should be in the subset?
    rows_for_subset <- Reduce(
      `&`, Map(`==`, dt[, col_and_row_vars, with = FALSE], value_list))
    # perform the given function on the subset of the data table
    return(dt[rows_for_subset, eval(parse(
      text = function_as_character))])
  })
  # check types of function outputs
  unique_types <- kim::su(unlist(lapply(function_output, typeof)))
  # if there are multiple types, return the list of outputs
  if (length(unique_types) > 1) {
    kim::pm(
      "Returning a list of outputs, because there were ",
      "multiple types of output from the function as follows: ",
       paste0(unique_types, collapse = ", "))
    return(function_output)
  }
  # as the outputs are only of one type, e.g., numeric or
  # character, unlist them
  unlisted_output <- unlist(function_output)
  # round the outputs if applicable
  if (is.numeric(unlisted_output)) {
    if (!is.null(sigfigs)) {
      unlisted_output <- signif(unlisted_output, sigfigs)
    }
  }
  # put the outputs in a matrix
  output_as_matrix <- matrix(unlisted_output, nrow = nrow(row_headers))
  # identify type of data and set appropriate type of na values
  na_value_for_filling <- data.table::fcase(
    is.numeric(output_as_matrix), NA_real_,
    is.character(output_as_matrix), NA_character_,
    is.complex(output_as_matrix), NA_complex_)
  # row headers and data
  row_headers_and_data <- rbind(
    c(as.list(row_names),
      as.list(rep(na_value_for_filling, ncol(output_as_matrix)))),
    data.table::data.table(row_headers, output_as_matrix)
  )
  # column headers as data table
  col_header_dt <- data.table::data.table(
    data.table::setDT(
      rep(list(rep(NA, length(col_headers))), length(row_headers) - 1)),
    names(col_headers),
    data.table::transpose(col_headers))
  # temporarily set column names
  names(col_header_dt) <- paste0("v", seq_along(col_header_dt))
  names(row_headers_and_data) <- paste0(
    "v", seq_along(row_headers_and_data))
  # put together the column headers, row headers, and data
  output_dt <- rbind(col_header_dt, row_headers_and_data)
  # remove column names
  if (remove_col_names == TRUE) {
    names(output_dt) <- rep("", length(output_dt))
  }
  return(output_dt)
}
