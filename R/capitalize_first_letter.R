#' Capitalize the first letter
#'
#' Capitalizes the first letter of a given character string
#'
#' @param x a character string
#' @return a character string
#' @examples
#' capitalize_first_letter("abc")
#' @export
# capitalize first letter
capitalize_first_letter <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}
