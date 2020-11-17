#' Proportion of given values in a vector
#'
#' @param vector a numeric or character vector
#' @param values a set of values
#' @param na.exclude if \code{TRUE}, NA values will be removed both from
#' \code{vector} and \code{values} before calculation
#' @examples
#' proportion_of_values_in_vector(
#' values = 2:3, vector = c(1:3, NA))
#' proportion_of_values_in_vector(
#' values = 2:3, vector = c(1:3, NA), na.exclude = FALSE)
#' proportion_of_values_in_vector(
#' values = c(2:3, NA), vector = c(1:3, NA), na.exclude = FALSE)
#' @export
proportion_of_values_in_vector <- function(
  values, vector, na.exclude = TRUE) {
  if (na.exclude == TRUE) {
    message(paste0(
      sum(is.na(values)), " of ", length(values), " values were NA."))
    message(paste0(
      sum(is.na(vector)), " of ", length(vector),
      " elements in the vector were NA."))
    values_with_no_na <- values[!is.na(values)]
    vector_with_no_na <- vector[!is.na(vector)]
    output <- sum(values_with_no_na %in% vector_with_no_na) /
      length(vector_with_no_na)
  } else {
    output <- sum(values %in% vector) / length(vector)
  }
  return(output)
}
