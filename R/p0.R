#' Paste0
#'
#' A shorthand for the function `paste0`
#' Concatenate vectors after converting to character.
#'
#' @param ... one or more R objects, to be converted to character vectors.
#' This is the same argument that would be used in the `paste0` function.
#' @param collapse an optional character string to separate the results.
#' Not NA_character_.
#' This is the same argument that would be used in the `paste0` function.
#' @param recycle0 logical indicating if zero-length character
#' arguments should lead to the zero-length character(0)
#' after the sep-phase (which turns into "" in the
#' collapse-phase, i.e., when collapse is not NULL).
#' This is the same argument that would be used in the `paste0` function.
#' @examples
#' paste0("a", "b")
#' p0("a", "b")
#' @export
#' @import data.table
p0 <- function(
  ...,
  collapse = NULL,
  recycle0 = FALSE) {
  paste0(... = ..., collapse = collapse, recycle0 = recycle0)
}
