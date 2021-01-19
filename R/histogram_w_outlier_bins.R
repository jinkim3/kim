#' Histogram with outlier bins
#'
#' Create a histogram with outlier bins
#'
#' @param vector a numeric vector
#' @param bin_cutoffs cutoff points for bins
#' @param outlier_bin_left logical. Should the leftmost bin treated
#' as an outlier bin? (default = TRUE)
#' @param outlier_bin_right logical. Should the rightmost bin treated
#' as an outlier bin? (default = TRUE)
#' @param x_tick_marks a vector of values at which to place tick marks
#' on the x axis. Note that the first bar spans from 0.5 to 1.5,
#' second bar from 1.5 to 2.5, ... nth bar from n - 0.5 to n + 0.5.
#' See the example. By default, tick marks will be placed at every
#' cutoff point for bins
#' @param x_tick_mark_labels a character vector to label tick marks.
#' By default, the vector of cutoff points for bins will also be
#' used as labels.
#' @param y_tick_marks a vector of values at which to place tick marks
#' on the y axis (e.g., setting \code{y_tick_marks = seq(0, 10, 5)} will
#' put tick marks at 0, 5, and 10.)
#' @param outlier_bin_fill_color color to fill inside of the
#' outlier bins (default = "coral")
#' @param non_outlier_bin_fill_color color to fill inside of the
#' non-outlier bins (default = "cyan4")
#' @param border_color color for borders of the bins (default = "black")
#' @param y_axis_title_vjust position of the y axis title (default = 0.85).
#' @param x_axis_title title for x axis (default = "Value"). If
#' \code{x_axis_title = FALSE}, x axis title will be removed from the plot.
#' @param y_axis_title title for y axis. By default, it will be either
#' "Proportion" or "Count".
#' @param notify_na_count if \code{TRUE}, notify how many observations
#' were removed due to missing values. By default, NA count will be printed
#' only if there are any NA values.
#' @param plot_proportion logical. Should proportions be plotted,
#' as opposed to frequencies? (default = TRUE)
#' @param plot_frequency logical. Should frequencies be plotted,
#' as opposed to proportions? (default = FALSE).
#' If \code{plot_frequency = TRUE}, \code{plot_proportion} will
#' switch to be FALSE.
#' @param mean logical. Should mean marked on the histogram?
#' (default = TRUE)
#' @param median logical. Should median marked on the histogram?
#' (default = TRUE)
#' @param median_position position of the median label as a percentage of
#' height of the tallest bin (default = 15)
#' @return a ggplot object
#' @examples
#' histogram_w_outlier_bin(vector = 1:100, bin_cutoffs = seq(0, 100, 10))
#' histogram_w_outlier_bin(vector = 0:89, bin_cutoffs = seq(0, 90, 10),
#' x_tick_marks = seq(0.5, 9.5, 3), x_tick_mark_labels = seq(0, 90, 30))
#' histogram_w_outlier_bin(vector = 1:10, bin_cutoffs = seq(0, 10, 2.5))
#' histogram_w_outlier_bin(vector = 1:5, bin_cutoffs = seq(0, 10, 2.5))
#' histogram_w_outlier_bin(vector = 1:15, bin_cutoffs = c(5.52, 10.5))
#' @import data.table ggplot2
#' @export
histogram_w_outlier_bin <- function(
  vector = NULL,
  bin_cutoffs = NULL,
  outlier_bin_left = TRUE,
  outlier_bin_right = TRUE,
  x_tick_marks = NULL,
  x_tick_mark_labels = NULL,
  y_tick_marks = NULL,
  outlier_bin_fill_color = "coral",
  non_outlier_bin_fill_color = "cyan4",
  border_color = "black",
  y_axis_title_vjust = 0.85,
  x_axis_title = NULL,
  y_axis_title = NULL,
  notify_na_count = NULL,
  plot_proportion = TRUE,
  plot_frequency = FALSE,
  mean = TRUE,
  median = TRUE,
  median_position = 15) {
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
  # check if bin_cutoffs argument is null
  if (is.null(bin_cutoffs)) {
    stop(paste0(
      "Please set cutoff points for bins by entering a numeric vector ",
      "for bin_cutoffs"))
  }
  # do bin_cutoffs include min and max values?
  # if not, add min and max values to bin_cutoffs
  if (min(v_no_na) < min(bin_cutoffs, na.rm = TRUE)) {
    bin_cutoffs <- c(min(v_no_na), bin_cutoffs)
  }
  if (max(v_no_na) > max(bin_cutoffs, na.rm = TRUE)) {
    bin_cutoffs <- c(bin_cutoffs, max(v_no_na))
  }
  # warn
  if (length(unique(diff(bin_cutoffs))) > 1) {
    warning(paste0(
      "\n\nBin widths are not identical.\n",
      "Be careful when comparing the areas of bars.\n"))
  }
  # characteristics of the histogram
  bin_number <- utils::head(seq_along(bin_cutoffs), - 1)
  bin_start <- utils::head(bin_cutoffs, -1)
  bin_end <- utils::tail(bin_cutoffs, -1)
  n_bins <- max(bin_number)
  # get count of each bin
  count <- vapply(bin_number, function(i) {
    if (i < n_bins) {
      sum(v_no_na >= bin_start[i] & v_no_na < bin_end[i])
    } else {
      sum(v_no_na >= bin_start[i] & v_no_na <= bin_end[i])
    }
  }, FUN.VALUE = numeric(1L))
  # get proportion of each bin
  proportion <- count / sum(count)
  # create a data table
  dt <- data.table(
    bin_number, bin_start, bin_end, count, proportion)
  # plot frequency or proportion? set a default
  if (plot_frequency == TRUE) {
    plot_proportion <- FALSE
    message(paste0(
      "Plotting frequencies instead of proportions because ",
      "plot_frequency = TRUE"))
  }
  y <- fcase(
    plot_proportion == TRUE, "proportion",
    plot_frequency == TRUE, "frequency")
  # fill colors for bins
  fill_colors <- rep(non_outlier_bin_fill_color, n_bins)
  if (outlier_bin_left == TRUE) {
    fill_colors[1] <- outlier_bin_fill_color
  }
  if (outlier_bin_right == TRUE) {
    fill_colors[n_bins] <- outlier_bin_fill_color
  }
  # plot
  g1 <- ggplot(data = dt, aes(x = bin_number, y = get(y)))
  g1 <- g1 + geom_bar(
    stat = "identity",
    color = border_color,
    fill = fill_colors,
    width = 1)
  g1 <- g1 + kim::theme_kim(
    y_axis_title_vjust = y_axis_title_vjust)
  # label axes
  if (!is.null(x_axis_title)) {
    if (x_axis_title == FALSE) {
      g1 <- g1 + theme(axis.title.x = element_blank())
    } else {
      g1 <- g1 + xlab(x_axis_title)
    }
  } else {
    g1 <- g1 + xlab("Value")
  }
  if (!is.null(y_axis_title)) {
    g1 <- g1 + ylab(y_axis_title)
  } else {
    g1 <- g1 + ylab(kim::capitalize(y))
  }
  # adjust x axis tick marks
  if (!is.null(x_tick_marks) & is.null(x_tick_mark_labels)) {
    message("Setting x_tick_mark_labels = x_tick_marks...")
    x_tick_mark_labels <- x_tick_marks
  }
  if (is.null(x_tick_marks)) {
    x_tick_marks <- seq(0.5, n_bins + 0.5, 1)
  }
  if (is.null(x_tick_mark_labels)) {
    x_tick_mark_labels <- bin_cutoffs
  }
  g1 <- g1 + scale_x_continuous(
    breaks = x_tick_marks,
    labels = x_tick_mark_labels
  )
  # update y tick marks
  if (!is.null(y_tick_marks)) {
    g1 <- g1 + scale_y_continuous(
      limits = c(
        min(y_tick_marks, na.rm = TRUE),
        max(y_tick_marks, na.rm = TRUE)),
      breaks = y_tick_marks)
  }
  # actual bin cutoffs, which are different from cutoffs that are
  # displayed on the plot
  actual_bin_cutoffs <- seq_len(n_bins + 1) - 0.5
  # get the x coordinate for mean
  mean_x_coordinate <- rel_value_of_pos_in_vector(
    vector = actual_bin_cutoffs,
    position = rel_pos_of_value_in_vector(mean(v_no_na), bin_cutoffs))
  # get the x coordinate for median
  median_x_coordinate <- rel_value_of_pos_in_vector(
    vector = actual_bin_cutoffs,
    position = rel_pos_of_value_in_vector(median(v_no_na), bin_cutoffs))
  # get the x coordinate for lower and upper limits of 95% ci
  ci_95_ll = tryCatch(
    as.numeric(stats::t.test(v_no_na)[["conf.int"]][1]),
    warning = function(w) NA_real_, error = function(e) NA_real_)
  ci_95_ul = tryCatch(
    as.numeric(stats::t.test(v_no_na)[["conf.int"]][2]),
    warning = function(w) NA_real_, error = function(e) NA_real_)
  ci_95_ll_x_coordinate <- rel_value_of_pos_in_vector(
    vector = actual_bin_cutoffs,
    position = rel_pos_of_value_in_vector(ci_95_ll, bin_cutoffs))
  ci_95_ul_x_coordinate <- rel_value_of_pos_in_vector(
    vector = actual_bin_cutoffs,
    position = rel_pos_of_value_in_vector(ci_95_ul, bin_cutoffs))
  # mark 95% ci
  g1 <- g1 + geom_errorbarh(
    aes(
      xmin = ci_95_ll_x_coordinate,
      xmax = ci_95_ul_x_coordinate,
      y = 0),
    size = 3,
    height = (max(dt[, get(y)]) - 0) * median_position / 100,
    color = "black")
  # mark mean
  g1 <- g1 + geom_point(
    data = data.frame(mean_x_coordinate),
    aes(x = mean_x_coordinate, y = 0),
    size = 5, color = "black")
  # mark median
  g1 <- g1 + geom_text(
    data = data.frame(median_x_coordinate),
    aes(x = median_x_coordinate,
        y = (max(dt[, get(y)]) - 0) * median_position / 100,
        label = "Mdn\nX"),
    fontface = "bold", hjust = 0.5, vjust = 0.5,
    size = 7, color = "black")
  return(g1)
}
