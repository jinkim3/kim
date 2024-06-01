#' correlation matrix
#'
#' Creates a correlation matrix
#'
#' @param data a data object (a data frame or a data.table)
#' @param var_names names of the variables for which to calculate
#' all pairwise correlations
#' @param row_var_names names of the variables that will go on the rows
#' of the correlation matrix
#' @param col_var_names names of the variables that will go on the columns
#' of the correlation matrix
#' @param round_r number of decimal places to which to round
#' correlation coefficients (default = 2)
#' @param round_p number of decimal places to which to round
#' p-values (default = 3)
#' @param output_type which value should be filled in cells of the
#' correlation matrix? If \code{output_type = "r"}, correlation
#' coefficients; if \code{output_type = "p"}, p-values;
#' if \code{output_type = "rp"}, correlation coefficients with
#' significance symbols based on p-values;
#' if \code{output_type = "n"}, sizes of the samples used to calculate
#' the correlation coefficients. By default, \code{output_type = "rp"}
#' @param numbered_cols logical. If \code{numbered_cols == TRUE} and
#' if \code{identical(row_var_names, col_var_names) == TRUE}, then
#' the columns will be numbered instead of containing variable names.
#' @return the output will be a correlation matrix in a data.table format
#' @examples
#' correlation_matrix(data = mtcars, var_names = c("mpg", "cyl", "wt"))
#' correlation_matrix(data = mtcars,
#' row_var_names = c("mpg", "cyl", "hp"), col_var_names = c("wt", "am"))
#' correlation_matrix(
#' data = mtcars, var_names = c("mpg", "cyl", "wt"),
#' numbered_cols = FALSE)
#' )
#' @export
correlation_matrix <- function(
  data = NULL,
  var_names = NULL,
  row_var_names = NULL,
  col_var_names = NULL,
  round_r = 2,
  round_p = 3,
  output_type = "rp",
  numbered_cols = NULL) {
  # convert data to data table
  dt <- data.table::setDT(data.table::copy(data))
  # set rows and cols
  if (!is.null(var_names)) {
    row_var_names <- var_names
    col_var_names <- var_names
  }
  if (is.null(col_var_names)) {
    col_var_names <- row_var_names
  }
  if (is.null(row_var_names)) {
    row_var_names <- col_var_names
  }
  if (identical(row_var_names, col_var_names) &&
      is.null(numbered_cols)) {
    numbered_cols <- TRUE
  }
  if (identical(row_var_names, col_var_names) == FALSE) {
    numbered_cols <- FALSE
  }
  cols <- lapply(seq_along(col_var_names), function(i) {
    focal_col_var_name <- col_var_names[i]
    # fill each column
    col_i <- unlist(lapply(seq_along(row_var_names), function(j) {
      cor_result <- tryCatch(
        stats::cor.test(
          x = dt[, get(row_var_names[j])],
          y = dt[, get(focal_col_var_name)]),
        error = function(e) "error",
        warning = function(w) "warning")
      if (cor_result[1] %in% c("error", "warning")) {
        return(NA)
      }
      r <- round(
        cor_result[["estimate"]], round_r)
      p <- cor_result[["p.value"]]
      p_nice <- kim::pretty_round_p_value(
        p, round_digits_after_decimal = round_p)
      n <- cor_result[["parameter"]][["df"]] + 2
      rp <- data.table::fcase(
        r == 1, "1",
        p < .001, paste0(r, "***"),
        p < .01, paste0(r, "**"),
        p < .05, paste0(r, "*"),
        p < .1, paste0(r, " m.s."),
        default = as.character(r)
      )
      output <- ifelse(
        output_type == "r", r, ifelse(
          output_type == "p", p_nice, ifelse(
            output_type == "rp", rp, ifelse(
              output_type == "n", n, NULL))))
      return(output)
    }))
  })
  dt2 <- data.table::setDT(cols)
  names(dt2) <- col_var_names
  variable <- row_var_names
  dt2 <- data.table::data.table(variable, dt2)
  # number columns
  if (numbered_cols == TRUE) {
    # the row and col vars should match
    if (identical(row_var_names, col_var_names) == FALSE) {
      stop(paste0(
        "To use numbered columns, the variable names in the ",
        "row and\nthe variable names in the column should match."))
    }
    dt2 <- data.table::data.table(
      var_id = paste0("(", seq_along(row_var_names), ")"), dt2)
    names(dt2) <- c(
      "", "Variable", paste0("(", seq_along(col_var_names), ")"))
  }
  return(dt2)
}
