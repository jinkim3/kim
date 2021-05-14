#' Paste for message
#'
#' Combines the base functions paste0 and message
#'
#' @param ... one or more R objects, to be converted to character vectors.
#' Input(s) to this argument will be passed onto the paste0 function.
#' @param collapse an optional character string to separate the results.
#' Not `NA_character_`. Input(s) to this argument will be passed onto
#' the paste0 function.
#' @return there will be no output from this function. Rather, a message
#' will be generated from the arguments.
#' @examples
#' pm("hello", 123)
#' pm(c("hello", 123), collapse = ", ")
#' @export
pm <- function(
  ...,
  collapse = NULL) {
  message(paste0(list(...), collapse))
}
