#' Find relative value of a position in a vector
#'
#' Find relative value of a position in a vector
#'
#' @param position position of a vector
#' @param vector a numeric vector
#' @return a number indicating the relative value of the position in the
#' vector
#' @examples
#' rel_value_of_pos_in_vector(vector = c(0, 100), position = 1.5)
#' rel_value_of_pos_in_vector(vector = 2:4, position = 2)
#' rel_value_of_pos_in_vector(vector = c(2, 4, 6), position = 2.5)
#' @export
rel_value_of_pos_in_vector <- function(
  vector = NULL, position = NULL) {
  v_no_na <- sort(vector[!is.na(vector)])
  # check if the position number is positive
  if (!position > 0) {
    stop("The input for position must be positive.")
  }
  # check if the position is greater than the length of the vector
  if (position > length(v_no_na)) {
    stop("The input for position is greater than length of the vector.")
  }
  # check if a value exists at the focal position in the vector
  if (position %% 1 == 0 & length(v_no_na) >= position) {
    rel_value <- v_no_na[position]
    return(rel_value)
  }
  # two values in the vector whose positions are closes to the focal position
  left <- v_no_na[floor(position)]
  right <- v_no_na[ceiling(position)]
  # output
  rel_value <- (position %% 1) * (right - left) + left
  return(rel_value)
}
