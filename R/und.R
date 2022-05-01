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
#' # mean center
#' und(mean_center, 1:10)
#' # compare results with base function
#' scale(1:10, scale = TRUE)
#' # find the modes
#' und(mode, c(3, 3, 3, 1, 2, 2))
#' # return values that are not outliers
#' und(outlier_rm, c(12:18, 100))
#' kim::outlier(c(1:10, 100))
#' @export
#' @import data.table
und <- function(fn, ...) {
  # list of arguments entered
  # al stands for argument list
  al <- as.list(match.call(expand.dots = TRUE))
  # the code above returns the following list:
  # [[1]]und, $fn [function name], [[3]] [vector input etc]
  # change function name to a character
  fn <- as.character(al$fn)
  # the code above returns the function name, e.g., "corr_text"
  # remove the first two elements as we probably will not need them
  al[1:2] <- NULL
  # the code above returns a list of inputs,
  # e.g., [[1]]1:10, if the input was 1:10
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
  # round including trailing 0s
  if (fn == "round_t0") {
    # get the vector
    if ("x" %in% names(ae)) {
      x <- ae[["x"]]
    } else {
      x <- ae[[1]]
    }
    # set default values
    if (!"digits" %in% names(ae)) {
      ae$digits <- 2
    }
    output <- sprintf(
      fmt = paste0("%.", ae$digits, "f"), round(
        x, ae$digits))
    return(output)
  }
  # remove outliers
  if (fn == "outlier_rm") {
    # get the vector
    if ("x" %in% names(ae)) {
      x <- ae[["x"]]
    } else {
      x <- ae[[1]]
    }
    # set default values
    if (!"iqr" %in% names(ae)) {
      ae$iqr <- 1.5
    }
    outliers <- kim::outlier(x, iqr = ae$iqr)
    non_outlier_values <- x[which(!x %in% outliers)]
    return(non_outlier_values)
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
  # compare strings
  if (fn == "compare_strings") {
    # check whether the vector is character
    if (is.character(x) == FALSE) {
      stop("The input must be a character vector.")
    }
    # check whether the vector has a length greater than 1
    if (length(x) <= 1) {
      stop("The input vector must have more than one element.")
    }
    # check whether elements are identical
    identical_to_element_1 <- vapply(2:length(x), function(i) {
      identical(x[1], x[i])
    }, logical(1L))
    if (all(identical_to_element_1) == TRUE) {
      message("All elements of the input vector are identical.")
      return("all identical")
    }
    # print if lengths are all 80 or less
    string_lengths <- vapply(seq_along(x), function(i) {
      nchar(x[i])
    }, numeric(1L))
    # get the maximum length
    max_string_legnth <- max(string_lengths)
    if (max_string_legnth <= 80) {
      cat(x, sep = "\n")
    }
    # find the position where the elements differ
    for (i in seq_len(max(string_lengths))) {
      character_at_one_position <- vapply(seq_along(x), function(j) {
        substr(x[j], i, i)
      }, character(1L))
      identical_to_character_1 <- vapply(2:length(
        character_at_one_position), function(k) {
          identical(character_at_one_position[1], character_at_one_position[k])
        }, logical(1L))
      if (any(identical_to_character_1 == FALSE)) {
        position_of_difference <- i
        break
      }
    }
    if (max_string_legnth <= 80) {
      cat(paste0(c(
        rep("_", position_of_difference - 1),
        "^",
        " (Position ", i, ")\n"), collapse = ""))
    }
    # return the position of difference
    output <- list("position_of_difference" = i)
    return(output)
  }
  # starting from the line below, the input x must be a numeric vector
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
    unique_values <- unique(x)
    counts <- vapply(unique_values, function(value) {
      sum(x == value, na.rm = TRUE)
    }, numeric(1L))
    max_count <- max(counts, na.rm = TRUE)
    modes <- unique_values[which(counts == max_count)]
    return(modes)
  }
  # if nothing was returned by this point, the function must have been
  # incorrectly entered
  stop(paste0(
    "The function `", fn, "` is not one of the undocumented functions."))
}
