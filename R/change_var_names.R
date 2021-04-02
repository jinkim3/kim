#' Change variable names in a data set
#'
#' Change variable names in a data set
#'
#' @param data a data object (a data frame or a data.table)
#' @param old_var_names a vector of old variable names
#' (i.e., variable names to change)
#' @param new_var_names a vector of new variable names
#' @param skip_absent If \code{skip_absent = TRUE}, old variable names
#' that do not exist in the data set will be skipped (default = TRUE).
#' @param print_summary If \code{print_summary = TRUE}, a summary of
#' old and new variable names will printed. (default = TRUE)
#' @param output_type type of the output. If \code{output_type = "dt"},
#' the function's output will be a data.table with changed names.
#' If \code{output_type = "summary"}, the function's output will
#' be a data.table listing old and new variable names.
#' By default, \code{output_type = "dt"}.
#' @return a data.table object with changed variable names
#' @examples
#' change_var_names(
#' mtcars, old = c("mpg", "cyl"), new = c("mpg_new", "cyl_new"))
#' @export
change_var_names <- function(
  data = NULL,
  old_var_names = NULL,
  new_var_names = NULL,
  skip_absent = FALSE,
  print_summary = TRUE,
  output_type = "dt") {
  # check lengths of old and new names
  if (length(old_var_names) != length(new_var_names)) {
    stop(paste0("Please ensure that ",
    "`old_var_names` and `new_var_names` are of the same length."))
  }
  # convert to data.table
  dt <- data.table::setDT(data.table::copy(data))
  # set names
  dt <- data.table::setnames(
    dt, old = old_var_names, new = new_var_names,
    skip_absent = skip_absent)
  # print the summary table
  if (print_summary == TRUE) {
    summary_dt <- data.table(
      old_var_name = old_var_names, new_var_name = new_var_names)
    print(summary_dt)
  }
  # output
  if (output_type == "dt") {
    invisible(dt)
  } else if (output_type == "summary") {
    invisible(summary_dt)
  }
}
