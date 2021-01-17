#' print loop progress
#'
#' Print the current progress in a loop (e.g., for loop or lapply)
#'
#' @param iteration_number the current number of iteration
#' @param iteration_start the iteration number at which the loop begins
#' (default = 1)
#' @param iteration_end the iteration number at which the loop ends.
#' @param percent if \code{percent = 1}, progress level will be printed
#' at every 1 percent progress (default = 1)
#' @examples
#' for(i in seq_len(250)) {
#'   Sys.sleep(0.002)
#'   print_loop_progress(
#'     iteration_number = i,
#'     iteration_end = 250)
#' }
#' @export
print_loop_progress <- function(
  iteration_number = NULL,
  iteration_start = 1,
  iteration_end = NULL,
  percent = 1) {
  every_n_iterations <- round(
    (iteration_end - iteration_start + 1) * percent / 100)
  progress_marker_position <-
    seq(iteration_start, iteration_end, every_n_iterations)
  if (iteration_number == iteration_start) {
    cat("Loop Progress:   0% \r")
    utils::flush.console()
  }
  if (iteration_number %in% progress_marker_position) {
    number_of_space_to_add <- 3 - nchar(as.character(round(
      iteration_number / iteration_end * 100)))
    cat(paste0(
      "Loop Progress: ",
      paste0(rep(" ", number_of_space_to_add), collapse = ""),
      round(iteration_number / iteration_end * 100), "% \r"))
    utils::flush.console()
  }
  if (iteration_number == iteration_end) {
    cat("Loop Progress: 100%. Done!\n")
  }
}
