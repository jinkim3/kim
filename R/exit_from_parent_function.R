#' Exit from a Parent Function
#'
#' @param n the number of generations to go back (default = 1)
#' @param silent logical. If \code{silent = TRUE}, a message will be printed.
#' @param message message to print
#' @examples
#' fn1 <- function() {
#' print(1)
#' print(2)
#' }
#' fn1()
#' fn2 <- function() {
#' print(1)
#' exit_from_parent_function()
#' print(2)
#' }
#' fn2()
#' @export
exit_from_parent_function <- function(
  n = 1,
  silent = FALSE,
  message = "Exiting from a parent function") {
  if (silent == FALSE) {
    message(message)
  }
  do.call("return", list(NULL), envir = parent.frame(n))
}
