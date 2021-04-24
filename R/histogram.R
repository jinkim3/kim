#' Histogram
#'
#' Create a histogram
#'
#' @param vector a numeric vector
#' @param number_of_bins number of bins for the histogram (default = 30)
#' @param x_tick_marks a vector of values at which to place tick marks
#' on the x axis (e.g., setting \code{x_tick_marks = seq(0, 10, 5)} will
#' put tick marks at 0, 5, and 10.)
#' @param y_tick_marks a vector of values at which to place tick marks
#' on the y axis (e.g., setting \code{y_tick_marks = seq(0, 10, 5)} will
#' put tick marks at 0, 5, and 10.)
#' @param fill_color color for inside of the bins (default = "cyan4")
#' @param border_color color for borders of the bins (default = "black")
#' @param y_axis_title_vjust position of the y axis title (default = 0.85).
#' @param x_axis_title title for x axis (default = "Value")
#' @param y_axis_title title for y axis (default = "Count")
#' @param cap_axis_lines logical. Should the axis lines be capped at the
#' outer tick marks? (default = FALSE)
#' @param notify_na_count if \code{TRUE}, notify how many observations
#' were removed due to missing values. By default, NA count will be printed
#' only if there are any NA values.
#' @return the output will be a histogram, a ggplot object.
#' @examples
#' histogram(1:100)
#' histogram(c(1:100, NA))
#' histogram(vector = mtcars[["mpg"]])
#' histogram(vector = mtcars[["mpg"]], x_tick_marks = seq(10, 36, 2))
#' histogram(vector = mtcars[["mpg"]], x_tick_marks = seq(10, 36, 2),
#' y_tick_marks = seq(0, 8, 2), y_axis_title_vjust = 0.5,
#' y_axis_title = "Freq", x_axis_title = "Values of mpg")
#' @export
histogram <- function(
  vector = NULL,
  number_of_bins = 30,
  x_tick_marks = NULL,
  y_tick_marks = NULL,
  fill_color = "cyan4",
  border_color = "black",
  y_axis_title_vjust = 0.85,
  x_axis_title = NULL,
  y_axis_title = NULL,
  cap_axis_lines = FALSE,
  notify_na_count = NULL) {
  # check if Package 'ggplot2' is installed
  if (!"ggplot2" %in% rownames(utils::installed.packages())) {
    message(paste0(
      "This function requires the installation of Package 'ggplot2'.",
      "\nTo install Package 'ggplot2', type ",
      "'kim::prep(ggplot2)'",
      "\n\nAlternatively, to install all packages (dependencies) required ",
      "for all\nfunctions in Package 'kim', type ",
      "'kim::install_all_dependencies()'"))
    return()
  } else {
    # proceed if Package 'ggplot2' is already installed
    kim::prep("ggplot2")
  }
  # bind the vars locally to the function
  value <- bins <- NULL
  # deal with NA values
  v_no_na <- vector[!is.na(vector)]
  na_count <- length(vector) - length(v_no_na)
  # by default, notify only if NA values are present
  if (is.null(notify_na_count)) {
    notify_na_count <- ifelse(na_count > 0, TRUE, FALSE)
  }
  if (notify_na_count == TRUE) {
    message(paste0(
      "\n", na_count,
      " observation(s) were removed due to missing values.\n"
    ))
  }
  # create a data.frame
  data <- data.frame(value = v_no_na)
  # plot it
  g1 <- ggplot(data = data, aes(x = value))
  if (!is.null(x_tick_marks)) {
    if (length(unique(diff(x_tick_marks))) > 1) {
      warning(paste0(
        "The x tick marks are not equally distanced from one another.\n",
        "Be careful when comparing the areas of bars.\n"))
    }
    message(paste0(
      "Manually setting tick marks will ignore the argument for ",
      "number of bins."))
    g1 <- g1 + geom_histogram(
      breaks = x_tick_marks,
      fill = fill_color,
      color = border_color)
    # adjust x axis tick marks
    g1 <- g1 + scale_x_continuous(
      limits = c(
        min(x_tick_marks, na.rm = TRUE),
        max(x_tick_marks, na.rm = TRUE)),
      breaks = x_tick_marks)
  } else {
    g1 <- g1 + geom_histogram(
      bins = number_of_bins,
      fill = fill_color,
      color = border_color)
  }
  # update y tick marks
  if (!is.null(y_tick_marks)) {
    g1 <- g1 + scale_y_continuous(
      limits = c(
        min(y_tick_marks, na.rm = TRUE),
        max(y_tick_marks, na.rm = TRUE)),
      breaks = y_tick_marks)
  }
  # label axes
  if (!is.null(x_axis_title)) {
    g1 <- g1 + xlab(x_axis_title)
  } else {
    g1 <- g1 + xlab("Value")
  }
  if (!is.null(y_axis_title)) {
    g1 <- g1 + ylab(y_axis_title)
  } else {
    g1 <- g1 + ylab("Count")
  }
  g1 <- g1 + theme_kim(
    y_axis_title_vjust = y_axis_title_vjust,
    cap_axis_lines = cap_axis_lines)
  return(g1)
}
