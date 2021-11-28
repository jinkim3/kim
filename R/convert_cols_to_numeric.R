#' Convert columns to numeric
#'
#' Check whether each column in a data.table can be converted to numeric,
#' and if so, convert every such column.
#'
#' @param data a data object (a data frame or a data.table)
#' @param classes a character vector specifying classes of columns
#' that will be converted. For example, if \code{classes = "character"},
#' all columns of the class "character" will be converted--if they can be
#' converted. The current version of the function only supports converting
#' character columns to numeric.
#' @param warn_accuracy_loss logical. whether to warn the user if
#' converting character to numeric leads to loss of accuracy.
#' (default = TRUE)
#' @param print_summary If \code{print_summary = TRUE}, a summary of
#' converted columns will printed. (default = TRUE)
#' @param silent If \code{silent = FALSE}, a message regarding conversion
#' for a data.frame will be printed. If \code{silent = TRUE}, this message
#' will be suppressed. By default, \code{silent = FALSE}.
#' @examples
#' data_frame_1 <- data.frame(a = c("1", "2"), b = c("1", "b"), c = 1:2)
#' convert_cols_to_numeric(data = data_frame_1)
#' data_table_1 <- data.table::data.table(
#' a = c("1", "2"), b = c("1", "b"), c = 1:2)
#' convert_cols_to_numeric(data = data_table_1)
#' @export
convert_cols_to_numeric <- function(
  data = NULL,
  classes = "character",
  warn_accuracy_loss = TRUE,
  print_summary = TRUE,
  silent = FALSE) {
  # check data type
  if (data.table::is.data.table(data)) {
    data_type <- "data_table"
  } else if (is.data.frame(data)) {
    data_type <- "data_frame"
  } else {
    stop(paste0("The data are neither a data.frame nor a data.table. ",
                'Please check your input for the "data" argument.'))
  }
  # accepted classes
  accepted_classes <- "character"
  # check the classes argument
  if (length(setdiff(classes, accepted_classes)) > 0) {
    stop(paste0(
      "The current version of the function accepts only the ",
      "following class(es):\n", paste0(accepted_classes, collapse = ", ")))
  }
  # convert data to data.table
  dt <- data.table::setDT(data.table::copy(data))
  # store original column names
  col_name <- names(dt)
  # classes before conversion
  old_class <- vapply(dt, class, FUN.VALUE = character(1L))
  # warn accuracy loss when converting numbers?
  type_convert_numerals_arg <- ifelse(
    warn_accuracy_loss == TRUE, "warn.loss", "allow.loss")
  # convert to numeric
  stopifnot(is.list(dt))
  number_of_all_cols <- length(dt)
  number_of_char_cols_before <- sum(
    vapply(dt, class, FUN.VALUE = character(1L)) == "character")
  for (col in names(dt)) {
    data.table::set(dt, j = col, value = utils::type.convert(
      dt[[col]], classes = "character", how = "replace",
      as.is = TRUE, numerals = type_convert_numerals_arg))
  }
  # classes after conversion
  new_class <- vapply(dt, class, FUN.VALUE = character(1L))
  # print a summary
  if (print_summary == TRUE) {
    summary_dt <- data.table::data.table(
      col_name, old_class, new_class)[old_class != new_class]
    message("A summary of the converted columns:")
    print(summary_dt)
  }
  # convert to data frame if necessary
  if (data_type == "data_frame") {
    kim::pm(
      "The output of the function is a data.table copy of the input ",
      "data, which has been put through the conversion process ",
      "and has been converted back to a data.frame.")
    return(as.data.frame(dt))
  }
  # output
  return(dt)
}
