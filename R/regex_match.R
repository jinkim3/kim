#' Regular expression matches
#'
#' Returns elements of a character vector that match the given
#' regular expression
#'
#' @param regex a regular expression
#' provided, a default theme will be used.
#' @param vector a character vector in which to search for regular expression
#' matches
#' @param mute_report logical. Should the report on regular expression
#' matches be printed?
#' @param perl logical. Should Perl-compatible regexps be used?
#'
#' @examples
#' regex_match("p$", names(mtcars))
#' \\donttest{
#' colnames_ending_with_p <- regex_match("p$", names(mtcars))
#' }
#' @export
#' @import ggplot2
regex_match <- function(
  regex = NULL, vector = NULL,
  mute_report = FALSE, perl = FALSE) {
  output <- vector[grepl(regex, vector, perl = perl)]
  if (mute_report == FALSE) {
    message(paste0(
      "A total of ", length(output), " matches were found:"))
    cat(output)
  }
  invisible(output)
}
