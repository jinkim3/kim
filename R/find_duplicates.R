#' Find duplicated values in a vector
#'
#' Find duplicated values in a vector
#'
#' @param vector a vector whose elements will be checked for duplicates
#' @param na.rm logical. If \code{na.rm = TRUE}, NA values in the vector
#' will be removed before searching for duplicates.
#' If \code{na.rm = FALSE}, NA values will be included in the search
#' as potentially duplicated values.
#' @param output type of output. If \code{output = "summary"}, the function's
#' output will be a data.table summarizing duplicated values and
#' their counts. If \code{output = "duplicated_values"}, the function's
#' output will be a vector of duplicated values.
#' If \code{output = "non_duplicated_values"}, the function's output will
#' be a vector of non-duplicated values (default = "summary")
#' @param sigfigs number of significant digits to round to in
#' the percent column of the summary (default = 2)
#' @return the output will be a data.table object (summary),
#' a vector of duplicated values, or a vector non-duplicated values.
#' @examples
#' find_duplicates(mtcars$cyl)
#' find_duplicates(mtcars$cyl, output = "duplicated_values")
#' find_duplicates(vector = c(mtcars$cyl, 11:20, NA, NA))
#' find_duplicates(vector = c(mtcars$cyl, 11:20, NA, NA), na.rm = FALSE)
#' find_duplicates(vector = c(mtcars$cyl, 11:20, NA, NA),
#' na.rm = FALSE, sigfigs = 4, output = "duplicated_values")
#' @import data.table
#' @export
find_duplicates <- function(
  vector = NULL,
  na.rm = TRUE,
  sigfigs = 2,
  output = "summary") {
  # bind the vars locally to the function
  count <- value <- percent <- NULL
  # remove na
  if (na.rm == TRUE) {
    vector <- vector[!is.na(vector)]
    # notify na value removal
    na_count <- sum(is.na(vector))
    if (na_count > 0) {
      message(paste0(
        "\n", na_count,
        " observation(s) were removed due to missing values.\n"))
    }
  }
  # summarize the vector
  dt1 <- kim::tabulate_vector(
    vector, na.rm = FALSE, total_included = FALSE)
  # total count of all values
  total_count <- sum(dt1[, count])
  # total count of non duplicated values
  non_dup_total_count <- sum(dt1[count == 1, count])
  # summary table
  summary_dt <- data.table(
    value = c(as.character(dt1[count > 1, value]),
              "..non_duplicated_values"),
    count = c(dt1[count > 1, count], non_dup_total_count))
  summary_dt[, percent := signif(count / total_count * 100, sigfigs)]
  # print summary
  print(summary_dt)
  cat("\n")
  # output
  if (output == "duplicated_values") {
    return(dt1[count > 1, value])
  }
  if (output == "non_duplicated_values") {
    return(dt1[count == 1, value])
  }
  if (output == "summary") {
    invisible(summary_dt)
  }
}
