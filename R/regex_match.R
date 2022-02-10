#' Regular expression matches
#'
#' Returns elements of a character vector that match the given
#' regular expression
#'
#' @param regex a regular expression
#' provided, a default theme will be used.
#' @param vector a character vector in which to search for regular
#' expression matches, or a data table whose column names will be searched
#' @param silent logical. If \code{silent = FALSE}, a report on regular
#' expression matches will be printed. If \code{silent = TRUE}, the report
#' on regular expression matches will not be printed.
#' By default, \code{silent = FALSE}
#' @param perl logical. Should Perl-compatible regexps be used?
#'
#' @examples
#' regex_match("p$", names(mtcars))
#' \donttest{
#' colnames_ending_with_p <- regex_match("p$", names(mtcars))
#' }
#' @export
regex_match <- function(
  regex = NULL, vector = NULL,
  silent = FALSE, perl = FALSE) {
  # check if the target input was a data table
  if (data.table::is.data.table(vector)) {
    vector <- names(vector)
  }
  output <- vector[grepl(regex, vector, perl = perl)]
  if (silent == FALSE) {
    message(paste0(
      "A total of ", length(output), " matches were found:"))
    cat(output)
    cat("\n")
  }
  invisible(output)
}
