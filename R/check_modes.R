#' Check modes of objects
#'
#' @param ... R objects.
#' @param mode_to_confirm The function will test whether each input is
#' of this mode. For example, \code{
#' check_modes(a, mode_to_confirm = "numeric")}, the function will check
#' whether the object `a` is numeric.
#' @examples
#' check_modes(T, mode_to_confirm = "logical")
#' check_modes(
#' TRUE, FALSE, 1L, 1:3, 1.1, c(1.2, 1.3), "abc", 1 + 2i, intToBits(1L),
#' mode_to_confirm = "numeric")
#' @export
check_modes <- function(
    ...,
    mode_to_confirm = NULL) {
  # check the key input
  if (is.null(mode_to_confirm)) {
    stop(paste0(
      "Please specify the mode,\n",
      'e.g., check_modes(1, mode_to_confirm = "numeric")'))
  }
  # check the key input
  if (is.character(mode_to_confirm) == FALSE) {
    stop(paste0(
      'Please specify the mode as a character ("logical" below),\n',
      'e.g., check_modes(T, mode_to_confirm = "logical")'))
  }
  # list of objects ----
  inputs <- list(...)
  modes <- unlist(lapply(inputs, mode), recursive = FALSE)
  # data table indicating which objects are not of the focal type
  input_labels <- as.character(
    as.list(match.call(expand.dots = FALSE))[["..."]])
  dt <- data.table::data.table(object = input_labels, mode = modes)
  # check if all objects are of the focal mode
  if (all(modes == mode_to_confirm)) {
    all_obj_are_of_focal_mode <- TRUE
    summary <- paste0(
      "All inputs are of the mode ", mode_to_confirm, ".")
  } else {
    all_obj_are_of_focal_mode <- FALSE
    summary <- paste0(
      "Not all inputs are of the mode ", mode_to_confirm, ".")
    dt <- dt[mode != mode_to_confirm]
  }
  output <- list(
    logical = all_obj_are_of_focal_mode,
    summary = summary,
    dt = dt)
  return(output)
}
