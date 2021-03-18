#' Theme Kim
#'
#' A custom ggplot theme
#'
#' @param legend_position position of the legend (default = "none")
#' @param base_size base font size
#' @param axis_tick_font_size font size for axis tick marks
#' @param axis_title_font_size font size for axis title
#' @param y_axis_title_vjust position of the y axis title (default = 0.85).
#' If default is used, \code{y_axis_title_vjust = 0.85}, the y axis title
#' will be positioned at 85% of the way up from the bottom of the plot.
#' @param axis_title_margin_size size of the margin between axis title
#' and the axis line
#' @param cap_axis_lines logical. Should the axis lines be capped at the
#' outer tick marks? (default = TRUE)
#' @return a ggplot object; there will be no meaningful output from
#' this function. Instead, this function should be used with another
#' ggplot object, e.g., \code{
#' ggplot(mtcars , aes(x = disp, y = mpg)) + theme_kim()}
#' @examples
#' \donttest{
#' prep(ggplot2)
#' ggplot2::ggplot(mtcars, aes(x = cyl, y = mpg)) + geom_point() + theme_kim()
#' }
#' @export
#' @importFrom ggplot2 theme
theme_kim <- function(
  legend_position = "none",
  base_size = 20,
  axis_tick_font_size = 20,
  axis_title_font_size = 24,
  y_axis_title_vjust = 0.85,
  axis_title_margin_size = 24,
  cap_axis_lines = TRUE) {
  # create a theme based theme_classic
  theme_object <-
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
  # output so far
  output <- theme_object
  # cap the axis lines to the outer ticks
  if (cap_axis_lines == TRUE) {
    # https://github.com/stefanedwards/lemon/issues/26
    # refer to README in the package 'lemon'
    theme_object <- theme_object +
      theme(panel.border = element_blank(), axis.line = element_line())
    output <- list(
      theme_object,
      lemon::coord_capped_cart(left = "both", bottom = "both"))
  }
  return(output)
}
