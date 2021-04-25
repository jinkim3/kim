#' Theme Kim
#'
#' A custom ggplot theme
#'
#' If a axis lines are to be capped at the ends, the following package(s)
#' must be installed prior to running the function:
#' Package 'lemon' v0.4.4 (or possibly a higher version) by
#' Edwards et al. (2020),
#' <https://cran.r-project.org/package=lemon>
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
#' outer tick marks? (default = FALSE)
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
theme_kim <- function(
  legend_position = "none",
  base_size = 20,
  axis_tick_font_size = 20,
  axis_title_font_size = 24,
  y_axis_title_vjust = 0.85,
  axis_title_margin_size = 24,
  cap_axis_lines = FALSE) {
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
    kim::prep("ggplot2", silent_if_successful = TRUE)
  }
  # If cap_axis_lines == TRUE, check whether Package 'lemon' is installed
  if (cap_axis_lines == TRUE) {
    if (!"lemon" %in% rownames(utils::installed.packages())) {
      message(paste0(
        "To cap axis lines at the end(s), Package 'lemon' must ",
        "be installed.\nTo install Package 'lemon', type ",
        "'kim::prep(lemon)'",
        "\n\nAlternatively, to install all packages (dependencies) required ",
        "for all\nfunctions in Package 'kim', type ",
        "'kim::install_all_dependencies()'"))
      return()
    } else {
      # proceed if Package 'lemon' is already installed
      coord_cap_fn_from_lemon <- utils::getFromNamespace(
        "coord_capped_cart", "lemon")
    }
  }
  # create a theme based theme_classic
  theme_object <-
    ggplot2::theme_classic(base_size = base_size) %+replace% ggplot2::theme(
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
      ggplot2::theme(
        panel.border = element_blank(), axis.line = element_line())
    output <- list(
      theme_object,
      coord_cap_fn_from_lemon(left = "both", bottom = "both"))
  }
  return(output)
}
