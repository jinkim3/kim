#' Capitalize a substring
#'
#' Capitalizes the first letter (by default) or a substring
#' of a given character string or each element of the character vector
#'
#' @param x a character string or a character vector
#' @param start starting position of the susbtring (default = 1)
#' @param end ending position of the susbtring (default = 1)
#' @return a character string or a character vector
#' @examples
#' capitalize("abc")
#' capitalize(c("abc", "xyx"), start = 2, end = 3)
#' @export
# capitalize first letter
capitalize <- function(x, start = 1, end = 1) {
  if (length(x) == 1) {
    substr(x, start, end) <- toupper(substr(x, start, end))
    output <- x
  } else if (length(x) > 1) {
    output <- vapply(x, function(element) {
      substr(element, start, end) <- toupper(substr(element, start, end))
      return(element)
    }, FUN.VALUE = character(1L))
    output <- unname(output)
  }
  return(output)
}
