#' Histogram
#'
#' Create a histogram based on the output of the `hist` function
#' in the `graphics` package.
#'
#' @param vector a numeric vector
#' @param breaks a numeric vector indicating breaks for the bins.
#' By default, no input is required for this argument.
#' @param counts a numeric vector containing counts for the bins
#' (i.e., heights of the bins). By default, no input is required
#' for this argument.
#' @param percent logical. If \code{percent = TRUE}, percentages
#' will be plotted rather than frequencies (default = FALSE).
#' @param bin_fill_color color of the area inside each bin
#' (default = "green4")
#' @param bin_border_color color of the border around each bin
#' (default = "black")
#' @param bin_border_thickness thickness of the border around each bin
#' (default = 1)
#' @param notify_na_count if \code{TRUE}, notify how many observations
#' were removed due to missing values. By default, NA count will be printed
#' only if there are any NA values.
#' @param x_axis_tick_marks a vector of values at which to place tick marks
#' on the x axis (e.g., setting \code{x_axis_tick_marks = seq(0, 10, 5)} will
#' put tick marks at 0, 5, and 10.)
#' @param y_axis_tick_marks a vector of values at which to place tick marks
#' on the y axis (e.g., setting \code{y_axis_tick_marks = seq(0, 10, 5)} will
#' put tick marks at 0, 5, and 10.)
#' @param cap_axis_lines logical. Should the axis lines be capped at the
#' outer tick marks? (default = FALSE)
#' @param x_axis_title title for x axis (default = "Value")
#' @param y_axis_title title for y axis (default = "Count" or "Percentage",
#' depending on the value of \code{percent})
#' @param y_axis_title_vjust position of the y axis title (default = 0.85).
#' @return the output will be a histogram, a ggplot object.
#' @examples
#' \donttest{
#' histogram(1:100)
#' histogram(c(1:100, NA))
#' histogram(vector = mtcars[["mpg"]])
#' histogram(vector = mtcars[["mpg"]], percent = TRUE)
#' histogram(vector = mtcars[["mpg"]],
#' x_axis_tick_marks = c(10, 25, 35), y_axis_title_vjust = 0.5,
#' y_axis_title = "Freq", x_axis_title = "Values of mpg")
#' }
#' @export
histogram <- function(
    vector = NULL,
    breaks = NULL,
    counts = NULL,
    percent = FALSE,
    bin_fill_color = "green4",
    bin_border_color = "black",
    bin_border_thickness = 1,
    notify_na_count = NULL,
    x_axis_tick_marks = NULL,
    y_axis_tick_marks = NULL,
    cap_axis_lines = TRUE,
    x_axis_title = "Value",
    y_axis_title = NULL,
    y_axis_title_vjust = 0.85) {
  # bind the vars locally to the function
  xmin <- xmax <- ymin <- ymax <- NULL
  if (!is.null(vector)) {
    # deal with NA values
    v_no_na <- vector[!is.na(vector)]
    na_count <- length(vector) - length(v_no_na)
    # by default, notify only if NA values are present
    if (is.null(notify_na_count)) {
      notify_na_count <- ifelse(na_count > 0, TRUE, FALSE)
    }
    if (notify_na_count == TRUE) {
      message(paste0(
        na_count, " observation(s) were removed due to missing values."))
    }
    # get bin heights etc from hist
    hist_results <- graphics::hist(vector)
    # frequncies or percentages
    if (percent == TRUE) {
      breaks <- hist_results$breaks
      counts <- hist_results$counts / sum(hist_results$counts) * 100
      y_axis_title <- "Percentage"
    } else {
      if (is.null(breaks) & is.null(counts)) {
        breaks <- hist_results$breaks
        counts <- hist_results$counts
        y_axis_title <- "Count"
      }
    }
  }
  if (!is.null(breaks) | !is.null(counts)) {
    # check the lengths of breaks and counts
    if (length(breaks) - length(counts) != 1) {
      stop(paste0(
        "Please check your inputs. The length of the vector for ",
        "the `breaks` argument\nmust be greater than the length",
        " of the vector for the `counts` argument by 1."))
    }
  }
  # set plot area
  df <- data.frame(
    rect_id = seq_along(counts),
    xmin = utils::head(breaks, -1),
    xmax = utils::tail(breaks, -1),
    ymin = rep(0, length(counts)),
    ymax = counts,
    bin_fill_color = bin_fill_color,
    bin_border_color = bin_border_color,
    bin_border_thickness = bin_border_thickness)
  # draw a rectangle for each count
  g1 <- ggplot2::ggplot(data = df)
  g1 <- g1 + ggplot2::geom_rect(
    ggplot2::aes(xmin = xmin, xmax = xmax,
                 ymin = ymin, ymax = ymax),
    fill = bin_fill_color,
    color = bin_border_color,
    linewidth = bin_border_thickness)
  # x axis tick marks
  if (!is.null(x_axis_tick_marks)) {
    g1 <- g1 + ggplot2::scale_x_continuous(
      limits = range(breaks),
      breaks = x_axis_tick_marks)
  }
  # y axis tick marks
  if (!is.null(y_axis_tick_marks)) {
    g1 <- g1 + ggplot2::scale_y_continuous(
      breaks = y_axis_tick_marks)
  }
  # axis titles
  g1 <- g1 + ggplot2::labs(x = x_axis_title, y = y_axis_title)
  g1 <- g1 + kim::theme_kim(
    y_axis_title_vjust = y_axis_title_vjust,
    cap_axis_lines = cap_axis_lines)
  return(g1)
}
