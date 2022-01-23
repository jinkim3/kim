#' Undocumented functions
#'
#' A collection of miscellaneous functions lacking documentations
#'
#' @param fn name of the function
#' @param ... arguments for the function
#' @return the output will vary by function
#' @examples
#' # correlation
#' und(corr_text, x = 1:5, y = c(1, 2, 2, 2, 3))
#' und(mean_center, 1:10)
#' scale(1:10, scale = TRUE)
#' v3 <- 1:10
#' und(mean_center, v3)
#' @export
#' @import data.table
und <- function(fn, ...) {
  # list of arguments entered
  # al stands for argument list
  al <- as.list(match.call(expand.dots = TRUE))
  # change function name to a character
  fn <- as.character(al$fn)
  # remove the first two elements as we probably will not need them
  al[1:2] <- NULL
  # environment for evaluating language
  focal_environment <- new.env(parent = parent.frame())
  # evaluate languages
  # ae stands for arguments evaluated
  ae <- lapply(al, eval, envir = focal_environment)
  # corr text
  if (fn == "corr_text") {
    if (all(c("x", "y") %in% names(ae))) {
      cr <- stats::cor.test(x = ae$x, y = ae$y)
      # round r
      if (!"round_r" %in% names(ae)) {
        ae$round_r <- 2
      }
      r <- kim::pretty_round_r(
        cr$estimate, round_digits_after_decimal = ae$round_r)
      # df
      df <- cr$parameter[["df"]]
      # round p
      if (!"round_p" %in% names(ae)) {
        ae$round_p <- 3
      }
      p <- kim::pretty_round_p_value(
        cr$p.value, round_digits_after_decimal = ae$round_p,
        include_p_equals = TRUE)
      # output
      output <- paste0("r(", df, ") = ", r, ", ", p)
      return(output)
    } else {
      stop("Please provide arguments for the function `cor.test`")
    }
  }
  # confirm that only one vector is entered
  if (length(ae) == 1) {
    x <- ae[[1]]
  } else if ("x" %in% names(ae)) {
    x <- ae[["x"]]
  } else {
    stop(paste0(
      "There must be only one input, or the input must be entered ",
      "as follows: x = [input]"))
  }
  if (is.numeric(x) == FALSE) {
    stop("Please enter a numeric vector as an input.")
  }
  # mean center, standardize, z_score
  if (fn == "mean_center") {
    output <- scale(x = x, scale = FALSE)
    # if the output is a vector, return the vector
    if (dim(output)[2] == 1) {
      output <- as.vector(output)
    }
    return(output)
  }
  if (fn %in% c("standardize", "z_score")) {
    output <- scale(x = x, scale = TRUE)
    # if the output is a vector, return the vector
    if (dim(output)[2] == 1) {
      output <- as.vector(output)
    }
    return(output)
  }
  # mode
  if (fn == "mode") {
    xt <- table(x)
    max_count <- max(xt)
    modes <- unname(which(xt == max_count))
    return(modes)
  }
  # if nothing was returned by this point, the function must have been
  # incorrectly entered
  stop(paste0(
    "The function `", fn, "` is not one of the undocumented functions."))
}
