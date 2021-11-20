#' Vlookup
#'
#' Look up values in a reference data.table and return values
#' associated with the looked-up values contained in the reference data.table
#'
#' @param lookup_values a vector of values to look up
#' @param reference_dt a data.table containing the values to look up
#' as well as values associated with the looked-up values
#' that need to be returned.
#' @param col_name_for_lookup_values in the reference data.table,
#' name of the column containing \code{lookup_values}.
#' @param col_name_for_output_values in the reference data.table,
#' name of the column containing values to return (i.e., values
#' associated with the looked-up values that will be the function's output)
#' @examples
#' vlookup(lookup_values = c(2.620, 2.875), reference_dt = mtcars[1:9, ],
#' col_name_for_lookup_values = "wt", col_name_for_output_values = "qsec")
#' @export
vlookup <- function(
  lookup_values = NULL,
  reference_dt = NULL,
  col_name_for_lookup_values = NULL,
  col_name_for_output_values = NULL) {
  # convert reference_dt if necessary; throw an error if not possible
  if (data.table::is.data.table(reference_dt) == FALSE) {
    ref <- tryCatch(
      data.table::setDT(data.table::copy(reference_dt)),
      error = function(e) stop(paste0(
        "The input for the reference data.table is not, or cannot be ",
        "converted to, a data.table. Please check this input.")))
  } else {
    # copy reference_dt as a data.table called ref
    ref <- data.table::copy(reference_dt)
  }
  # make sure that the necessary columns exist in the reference_dt
  if (col_name_for_lookup_values %in% names(ref) == FALSE) {
    stop(paste0(
      'The column "', col_name_for_lookup_values, '" does not exist ',
    "in the reference_dt."))
  }
  if (col_name_for_output_values %in% names(ref) == FALSE) {
    stop(paste0(
      'The column "', col_name_for_output_values, '" does not exist ',
      "in the reference_dt."))
  }
  # subset reference data table to only the necessary columns
  ref <- ref[, c(
    col_name_for_lookup_values, col_name_for_output_values), with = FALSE]
  # change column names for ref
  # lv stands for lookup values
  names(ref) <- c("lv", "output")
  # check for duplicated values in the reference table's column
  # containing lookup_values
  if (any(duplicated(ref[["lv"]])) == TRUE) {
    message("There are duplicated values in the reference table's ",
            "column containing values to look up:")
    kim::find_duplicates(ref[["lv"]])
    stop(paste0(
      'Please ensure that there are no duplicated values in "',
      col_name_for_lookup_values, '" column of the reference_dt.'))
  }
  # check the reference table's column containing lookup_values
  if (any(lookup_values %in% ref[["lv"]]) == FALSE) {
    stop(paste0(
      'None of the values to look up exist in "',
      col_name_for_lookup_values, '" column of the reference_dt.'))
  }
  # lv stands for lookup values
  lv <- data.table::data.table(lv = lookup_values)
  # merge data tables to return desired values
  lv[ref, output := output, on = .(lv = lv)]
  # output
  return(lv[["output"]])
}
