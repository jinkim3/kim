#' ggsave quick
#'
#' quickly save the current plot with a timestamp
#'
#' @param name a character string of the png file name (default = "ggplot")
#' If the desired output file name is "myplot.png",
#' enter \code{name = "myplot", timestamp = FALSE}
#' @param timestamp if \code{timestamp = TRUE}, a timestamp of the
#' current time will be appended to the file name.
#' The timestamp will be in the format, jan_01_2021_1300_10_000001,
#' where "jan_01_2021" would indicate January 01, 2021;
#' 1300 would indicate 13:00 (i.e., 1 PM); and 10_000001 would
#' indicate 10.000001 seconds after the hour.
#' @param file_name_extension file name extension (default = ".png")
#' @param w width of the plot to be saved. This argument will be
#' directly entered as the \code{width} argument for the \code{ggsave}
#' function within \code{ggplot2} package (default = 16)
#' @param h height of the plot to be saved. This argument will be
#' directly entered as the \code{height} argument for the \code{ggsave}
#' function within \code{ggplot2} package (default = 9)
#' @return the output will be a .png image file in the working directory.
#' @examples
#' \donttest{
#' kim::histogram(rep(1:30, 3))
#' ggsave_quick()
#' }
#' @export
ggsave_quick <- function(
  name = "ggplot",
  timestamp = TRUE,
  file_name_extension = ".png",
  w = 16,
  h = 9) {
  # create a timestamp
  if (timestamp == TRUE) {
    ts <- tolower(
      gsub("\\.", "_", format(Sys.time(), "_%b_%d_%Y_%H%M_%OS6")))
  } else {
    ts <- ""
  }
  # set file name
  file_name <- paste0(name, ts, file_name_extension)
  # save
  ggplot2::ggsave(filename = file_name, width = w, height = h)
}
