#' Excel formula, convert (to)
#'
#' Alias for the 'convert_to_excel_formula' function.
#' Convert elements of a character vector to Excel formulas to preserve
#' the character (string) format when opened in an Excel file.
#'
#' @param vector a character vector
#' @return the output will be a character vector formatted as an Excel
#' formula. For example, if an element in the input vector was \code{".500"},
#' this element will be converted to \code{=".500"}, which will show up as
#' ".500" in Excel, rather than as "0.5"
#' @examples
#' \dontrun{
#' # compare the two csv files below
#' # example 1
#' dt <- data.table::data.table(a = ".500")
#' data.table::fwrite(dt, "example1.csv") # the csv will show "0.5"
#' # example 2
#' dt <- data.table::data.table(a = excel_formula_convert(".500"))
#' data.table::fwrite(dt, "example2.csv") # the csv will show ".500"
#' }
#' @export
excel_formula_convert <- function(
  vector = NULL) {
  output <- paste0("=", dQuote(vector, q = FALSE))
  return(output)
}
