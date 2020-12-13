#' Theme Kim
#'
#' A custom ggplot theme
#'
#' @param theme_name name of the custom ggplot theme. If no name is
#' provided, a default theme will be used.
#' @param legend_position position of the legend (default = "none")
#' @param base_size base font size
#' @param axis_tick_font_size font size for axis tick marks
#' @param axis_title_font_size font size for axis title
#' @param y_axis_title_vjust position of the y axis title (default = 0.85).
#' If default is used, \code{y_axis_title_vjust = 0.85}, the y axis title
#' will be positioned at 85% of the way up from the bottom of the plot.
#' @param axis_title_margin_size size of the margin between axis title
#' and the axis line
#'
#' @examples
#' ggplot(mtcars, aes(x = cyl, y = mpg)) + geom_point() + theme_kim()
#' @export
#' @import ggplot2
theme_kim <- function(
  theme_name = NULL,
  legend_position = "none",
  base_size = 20,
  axis_tick_font_size = 20,
  axis_title_font_size = 24,
  y_axis_title_vjust = 0.85,
  axis_title_margin_size = 24) {
  theme_classic(base_size = base_size) %+replace% theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = legend_position,
    axis.title.x = element_text(margin = margin(
      t = axis_title_margin_size)),
    axis.title.y = element_text(
      angle = 0, vjust = y_axis_title_vjust,
      margin = margin(r = axis_title_margin_size)),
    axis.title = element_text(
      face = "bold", color = "black", size = axis_title_font_size),
    axis.text = element_text(
      face = "bold", color= "black", size = axis_tick_font_size),
    legend.title = element_text(
      face = "bold", color = "black", size = axis_title_font_size),
    legend.text = element_text(
      face = "bold", color= "black", size = axis_tick_font_size)
    )
}
