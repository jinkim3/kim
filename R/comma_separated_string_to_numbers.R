#' Convert a comma-separated string of numbers
#'
#' @param string a character string consisting of numbers
#' separated by commas
#' @return a character string
#' @examples
#' comma_separated_string_to_numbers("1, 2, 3,4,  5  6")
#' @export
comma_separated_string_to_numbers <- function(string) {
  output <- stats::na.omit(
    as.numeric(
      unlist(
        strsplit(
          gsub(" ", "", string), ","))))
  return(output)
}
