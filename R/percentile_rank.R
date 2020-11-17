#' Percentile rank
#'
#' Calculate percentile rank of each value in a vector
#'
#' @param vector a numeric vector
#' @examples
#' percentile_rank(1:5)
#' percentile_rank(1:10)
#' percentile_rank(1:100)
#' @export
# percentile rank
percentile_rank <- function(vector) {
  v_no_na <- vector[!is.na(vector)]
  number_of_values <- length(v_no_na)
  output <- vapply(vector, function(x) {
    # deal with NA values
    if (is.na(x)) return(NA)
    # c = count of all scores less than the score of interest
    c <- sum(v_no_na < x)
    # f = frequency of the score of interest
    f <- sum(v_no_na == x)
    percentile_rank <- (c + 0.5 * f) / number_of_values * 100
    return(percentile_rank)
  }, FUN.VALUE = numeric(1))
  return(output)
}
