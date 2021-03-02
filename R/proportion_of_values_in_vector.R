#' Proportion of given values in a vector
#'
#' @param vector a numeric or character vector
#' @param values a set of values
#' @param na.exclude if \code{TRUE}, NA values will be removed both from
#' \code{vector} and \code{values} before calculation
#' @param silent If \code{silent = TRUE}, no message will be printed
#' regarding number of NA values. (default = FALSE)
#' @examples
#' proportion_of_values_in_vector(
#'   values = 2:3, vector = c(1:3, NA)
#' )
#' proportion_of_values_in_vector(
#'   values = 2:3, vector = c(1:3, NA), na.exclude = FALSE
#' )
#' proportion_of_values_in_vector(
#'   values = c(2:3, NA), vector = c(1:3, NA), na.exclude = FALSE
#' )
#' @export
proportion_of_values_in_vector <- function(
  values, vector, na.exclude = TRUE, silent = FALSE) {
  if (na.exclude == TRUE) {
    if (silent == FALSE) {
      # notify na values
      message(paste0(
        'NA values in the "values" argument: ',
        sum(is.na(values)), " out of ", length(values)
      ))
      message(paste0(
        'NA values in the "vector" argument: ',
        sum(is.na(vector)), " out of ", length(vector)
      ))
      message(paste0(
        'NA values were removed from the "values" and "vector"',
        " arguments before calculating the proportion."))
    }
    values_with_no_na <- values[!is.na(values)]
    vector_with_no_na <- vector[!is.na(vector)]
    # number of hits
    hits <- sum(vapply(vector_with_no_na, function(x) {
      ifelse(x %in% values_with_no_na, TRUE, FALSE)
    }, FUN.VALUE = logical(1L)))
    # proportion
    output <- hits / length(vector_with_no_na)
  } else {
    # notify na values
    if (silent == FALSE) {
      message(paste0(
        'NA values in the "values" argument: ',
        sum(is.na(values)), " out of ", length(values)
      ))
      message(paste0(
        'NA values in the "vector" argument: ',
        sum(is.na(vector)), " out of ", length(vector)
      ))
      message(paste0(
        'NA values were counted as valid values in the "values" ',
        'and "vector" arguments when calculating the proportion.'))
    }
    # number of hits
    hits <- sum(vapply(vector, function(x) {
      ifelse(x %in% values, TRUE, FALSE)
    }, FUN.VALUE = logical(1L)))
    # proportion
    output <- hits / length(vector)
  }
  return(output)
}
