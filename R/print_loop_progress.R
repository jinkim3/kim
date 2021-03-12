#' print loop progress
#'
#' Print current progress inside a loop (e.g., for loop or lapply)
#'
#' @param iteration_number current number of iteration
#' @param iteration_start iteration number at which the loop begins
#' (default = 1)
#' @param iteration_end iteration number at which the loop ends.
#' @param text_before text to add before "Loop Progress..."
#' By default, it is set to be blank, i.e., \code{text_before = ""}
#' @param percent if \code{percent = 1}, progress level will be printed
#' at every 1 percent progress (default = 1)
#' @param output_method if \code{output_method = "cat"}, progress level
#' will be printed using the 'cat' function;
#' if \code{output_method = "return"}, progress level will be
#' returned as the output of the function (default = "cat")
#' @examples
#' for (i in seq_len(250)) {
#'   Sys.sleep(0.001)
#'   print_loop_progress(
#'     iteration_number = i,
#'     iteration_end = 250)
#' }
#' unlist(lapply(seq_len(7), function (i) {
#'   Sys.sleep(0.2)
#'   print_loop_progress(
#'     iteration_number = i,
#'     iteration_end = 7)
#'   return(i)
#' }))
#' @export
print_loop_progress <- function(
  iteration_number = NULL,
  iteration_start = 1,
  iteration_end = NULL,
  text_before = "",
  percent = 1,
  output_method = "cat") {
  # make sure every_n_iterations is at least 1
  every_n_iterations <- max(round(
    (iteration_end - iteration_start + 1) * percent / 100), 1)
  # iterations at which to report progress
  progress_marker_position <-
    seq(iteration_start, iteration_end, every_n_iterations)
  if (iteration_number == iteration_start) {
    # output text
    output_text <- paste0(text_before, "Loop Progress:   0% \r")
    if (output_method == "cat") {
      cat(output_text)
      utils::flush.console()
    } else if (output_method == "return") {
      return(output_text)
    }
  }
  if (iteration_number %in% progress_marker_position) {
    number_of_space_to_add <- 3 - nchar(as.character(round(
      iteration_number / iteration_end * 100)))
    # output text
    output_text <- paste0(
      text_before, "Loop Progress: ",
      paste0(rep(" ", number_of_space_to_add), collapse = ""),
      round(iteration_number / iteration_end * 100), "% \r")
    if (output_method == "cat") {
      cat(output_text)
      utils::flush.console()
    } else if (output_method == "return") {
      return(output_text)
    }
  }
  if (iteration_number == iteration_end) {
    output_text <- paste0(text_before, "Loop Progress: 100%. Done!\n")
    if (output_method == "cat") {
      cat(output_text)
    } else if (output_method == "return") {
      return(output_text)
    }
  }
}
