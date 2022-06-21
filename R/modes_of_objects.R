#' Find modes of objects
#'
#' @param ... R objects.
#' @return the output will be a data.table listing objects and their mods.
#' @examples
#' modes_of_objects(
#' TRUE, FALSE, 1L, 1:3, 1.1, c(1.2, 1.3), "abc", 1 + 2i, intToBits(1L))
#' @export
modes_of_objects <- function(..., mode_to_confirm = NULL) {
  # list of objects ----
  inputs <- list(...)
  modes <- unlist(lapply(inputs, mode), recursive = FALSE)
  input_labels <- as.character(
    as.list(match.call(expand.dots = FALSE))[["..."]])
  dt <- data.table::data.table(object = input_labels, mode = modes)
  return(dt)
}
