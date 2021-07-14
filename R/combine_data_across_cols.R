#' Combine data across columns
#'
#' Combine data across columns. If NA is the only value across all focal
#' columns for given row(s), NA will be returned for those row(s).
#'
#' @param data a data object (a data frame or a data.table)
#' @param cols a character vector containing names of columns, across
#' which to combine data
#' @return the output will be a numeric or character vector.
#' @examples
#' dt <- data.frame(v1 = c(1, NA), v2 = c(NA, 2))
#' dt
#' combine_data_across_cols(data = dt, cols = c("v1", "v2"))
#' dt <- data.frame(v1 = c(1, 2, NA), v2 = c(NA, 4, 3))
#' dt
#' combine_data_across_cols(data = dt, cols = c("v1", "v2"))
#' dt <- data.frame(v1 = c(1, NA, NA), v2 = c(NA, 2, NA))
#' dt
#' combine_data_across_cols(data = dt, cols = c("v1", "v2"))
#' @export
combine_data_across_cols <- function(
  data = NULL,
  cols = NULL) {
  # check if any of the input columns are missing in the data
  missing_cols <- cols[!(cols %in% names(data))]
  if (length(missing_cols) > 0) {
    kim::pm(c("The following column(s) were not in the data: ",
            paste0(missing_cols, collapse = ", ")))
    return()
  }
  # dt with only the focal columns
  dt <- data.table::setDT(data.table::copy(data))[, cols, with = FALSE]
  # count the number of na values in each row of the columns
  num_of_non_na_values_per_row <- rowSums(!is.na(dt))
  # combine across columns if there is only one non na value per row
  # across the focal columns
  if (all(num_of_non_na_values_per_row <= 1)) {
    new_vector <- dt[, .SD[[max(c(which(!is.na(.SD))), 1)]],
                     by = seq_len(nrow(dt))][[2]]
    return(new_vector)
  } else {
    message(paste0(
      "The following row(s) had more than one non NA values per row ",
      "across the focal columns. "))
    kim::pm("Row number(s): ", paste0(
      which(num_of_non_na_values_per_row > 1), collapse = ", "))
    print(dt[which(num_of_non_na_values_per_row > 1)])
    return()
  }
}
