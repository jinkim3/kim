#' Find relative position of a value in a vector
#'
#' Find relative position of a value in a vector that may or may not contain
#' the value
#'
#' @param value a value whose relative position is to be searched in a vector
#' @param vector a numeric vector
#' @return a number indicating the relative position of the value in the
#' vector
#' @examples
#' rel_pos_of_value_in_vector(value = 3, vector = c(2, 4))
#' rel_pos_of_value_in_vector(value = 3, vector = c(2, 6))
#' rel_pos_of_value_in_vector(value = 3, vector = 1:3)
#' @export
rel_pos_of_value_in_vector <- function(value = NULL, vector = NULL) {
  v_no_na <- sort(vector[!is.na(vector)])
  # check if the value is already in the vector
  if (value %in% v_no_na) {
    rel_pos <- mean(which(v_no_na == value))
    return(rel_pos)
  }
  # check if the value is within the range of the vector
  if (value < min(v_no_na)) {
    stop(paste0(
      "The value ", value, " is less than the minimum of the vector."))
  }
  if (value > max(v_no_na)) {
    stop("The value is greater than the maximum of the vector.")
  }
  # two values in the vector that are most adjacent to the focal value
  left <- max(v_no_na[v_no_na <= value])
  right <- min(v_no_na[v_no_na >= value])
  # output
  rel_pos <- which(v_no_na == left) + (value - left) / (right - left)
  return(rel_pos)
}
