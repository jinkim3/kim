#' Cohen's d as a function of sample size
#'
#' Plot Cohen's d as sample size increases.
#'
#' @param data a data object (a data frame or a data.table)
#' @param iv_name name of the independent variable (grouping variable)
#' @param dv_name name of the dependent variable (measure variable
#' of interest)
#' @param save_as_png if \code{save = TRUE}, the plot will be saved
#' as a PNG file.
#' @param png_name name of the PNG file to be saved. By default, the name
#' will be "cohen_d_over_n_" followed by a timestamp of the
#' current time.
#' The timestamp will be in the format, jan_01_2021_1300_10_000001,
#' where "jan_01_2021" would indicate January 01, 2021;
#' 1300 would indicate 13:00 (i.e., 1 PM); and 10_000001 would
#' indicate 10.000001 seconds after the hour.
#' @param xlab title of the x-axis for the histogram by group.
#' If \code{xlab = FALSE}, the title will be removed. By default
#' (i.e., if no input is given), \code{dv_name} will be used as
#' the title.
#' @param ylab title of the y-axis for the histogram by group.
#' If \code{ylab = FALSE}, the title will be removed. By default
#' (i.e., if no input is given), \code{iv_name} will be used as
#' the title.
#' @param width width of the plot to be saved. This argument will be
#' directly entered as the \code{width} argument for the \code{ggsave}
#' function within \code{ggplot2} package (default = 16)
#' @param height height of the plot to be saved. This argument will be
#' directly entered as the \code{height} argument for the \code{ggsave}
#' function within \code{ggplot2} package (default = 9)
#' @return the output will be a list of (1) ggplot object
#' (histogram by group) and (2) a data.table with Cohen's d by sample size
#' @examples
#' \dontrun{
#' cohen_d_over_n(data = mtcars, iv_name = "am", dv_name = "mpg")
#' }
#' @export
#' @import data.table
cohen_d_over_n <- function(
  data = NULL,
  iv_name = NULL,
  dv_name = NULL,
  save_as_png = FALSE,
  png_name = NULL,
  xlab = NULL,
  ylab = NULL,
  width = 16,
  height = 9) {
  # bind the vars locally to the function
  n <- ci_95_ll <- ci_95_ul <- NULL
  # dt for plotting
  dt <- data.table::setDT(data.table::copy(data))
  # only iv and dv
  dt <- dt[, c(iv_name, dv_name), with = FALSE]
  names(dt) <- c("x", "y")
  # first row in which the second level in iv appears
  d_results_list <- lapply(seq_len(nrow(dt)), function(i) {
    suppressMessages(tryCatch(
      kim::cohen_d(data = dt[seq_len(i)], iv_name = "x", dv_name = "y"),
      error = function(e) NA_real_,
      warning = function(w) NA_real_))
  })
  dt2 <- data.table::data.table(
    dt, data.table::data.table(do.call(rbind, d_results_list)))
  dt2[, n := seq_len(nrow(dt2))]
  # plot
  g1 <- ggplot2::ggplot(dt2, ggplot2::aes(
    x = n, y = cohen_d))
  g1 <- g1 + ggplot2::geom_hline(yintercept = 0, linetype = "dashed")
  g1 <- g1 + ggplot2::geom_point()
  g1 <- g1 + ggplot2::geom_errorbar(ggplot2::aes(
    x = n,
    ymin = ci_95_ll,
    ymax = ci_95_ul))
  g1 <- g1 + ggplot2::geom_line()
  g1 <- g1 + kim::theme_kim(cap_axis_lines = TRUE)
  if (is.null(ylab)) {
    g1 <- g1 + ggplot2::ylab("Cohen's d")
  } else if (ylab == FALSE) {
    g1 <- g1 + ggplot2::theme(axis.title.y = element_blank())
  } else {
    g1 <- g1 + ggplot2::ylab(ylab)
  }
  if (is.null(xlab)) {
    g1 <- g1 + ggplot2::xlab("Sample Size (Chronological)")
  } else if (xlab == FALSE) {
    g1 <- g1 + ggplot2::theme(axis.title.x = element_blank())
  } else {
    g1 <- g1 + ggplot2::xlab(xlab)
  }
  print(g1)
  # save as png
  if (save_as_png == TRUE | !is.null(png_name)) {
    kim::ggsave_quick(g1, png_name, width = width, height = height)
  }
  # output
  output_list <- list(g1, dt2)
  names(output_list) <- c("plot", "data")
  return(output_list)
}
