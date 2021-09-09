#' Remove from a vector
#'
#' Remove certain values from a vector
#'
#' @param values a single value or a vector of values which will be
#' removed from the target vector
#' @param vector a character or numeric vector
#' @param silent if \code{silent = FALSE}, a summary of values removed
#' will be printed; if \code{silent = TRUE}, such summary will not be
#' printed. By default, \code{silent = FALSE}
#' @return the output will be a vector with the given values removed.
#' @examples
#' remove_from_vector(values = 1, vector = 1:3)
#' remove_from_vector(values = NA, vector = c(1:3, NA))
#' remove_from_vector(values = c(1, NA), vector = c(1:3, NA))
#' remove_from_vector(values = 1:5, vector = 1:10)
#' @export
remove_from_vector <- function(
  values = NULL,
  vector = NULL,
  silent = FALSE) {
  # check whether the target vector is an atomic vector
  if (is.atomic(vector) == FALSE) {
    stop(paste0(
      "The input for the vector is not an atomic vector ",
      "(a vector with all elements of the same type). ",
      "Please enter a character or numeric input for the vector argument."))
  }
  # check whether the target vector is character or numeric
  if (is.character(vector) == FALSE & is.numeric(vector) == FALSE) {
    stop(paste0(
      "The input for the vector is neither a numeric nor a character ",
      "vector. Please enter a character or numeric input for the ",
      "vector argument."))
  }
  # if there is only one distinct value to remove
  if (length(values) == 1) {
    # if values is na
    if (is.na(values)) {
      output <- vector[!is.na(vector)]
      # print summary
      if (silent == FALSE) {
        count <- sum(is.na(vector))
        kim::pm("A total of ", count, " values were removed.")
      }
    } else {
      output <- vector[vector != values]
      # print summary
      if (silent == FALSE) {
        count <- sum(vector == values, na.rm = TRUE)
        kim::pm("A total of ", count, " values were removed.")
      }
    }
  }
  # if there are multiple values
  if (length(values) > 1) {
    output <- vector[!(vector %in% values)]
    # print summary
    if (silent == FALSE) {
      summary_dt <- data.table::data.table(value = values)
      summary_dt[, count := vapply(values, function(x) {
        if (!is.na(x)) {
          sum(vector == x, na.rm = TRUE)
        } else {
          sum(is.na(vector))
        }
      }, numeric(1L))]
      total_count <- sum(summary_dt[, count], na.rm = TRUE)
      summary_dt <- rbind(summary_dt, data.table::data.table(
        value = "..total", count = total_count))
      kim::pm(
        "A total of ", total_count, " values were removed:")
      print(summary_dt)
    }
  }
  return(output)
}
