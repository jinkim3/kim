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
#' @param legend_spacing_y vertical spacing of the legend keys in
#' the unit of "cm" (default = 1)
#' @param legend_key_size size of the legend keys in the unit of "lines"
#' (default = 3)
#' @param base_size base font size
#' @param axis_tick_font_size font size for axis tick marks
#' @param axis_tick_marks_color color of the axis tick marks
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
#' ggplot2::ggplot(mtcars, aes(x = cyl, y = mpg)) +
#' geom_point() + theme_kim()
#' }
#' @export
theme_kim <- function(
  legend_position = "none",
  legend_spacing_y = 1,
  legend_key_size = 3,
  base_size = 20,
  axis_tick_font_size = 20,
  axis_tick_marks_color = "black",
  axis_title_font_size = 24,
  y_axis_title_vjust = 0.85,
  axis_title_margin_size = 24,
  cap_axis_lines = FALSE) {
  # installed packages
  installed_pkgs <- rownames(utils::installed.packages())
  # check if Package 'ggplot2' is installed
  if (!"ggplot2" %in% installed_pkgs) {
    message(paste0(
      "This function requires the installation of Package 'ggplot2'.",
      "\nTo install Package 'ggplot2', type ",
      "'kim::prep(ggplot2)'",
      "\n\nAlternatively, to install all packages (dependencies) required ",
      "for all\nfunctions in Package 'kim', type ",
      "'kim::install_all_dependencies()'"))
    return()
  }
  # If cap_axis_lines == TRUE, check whether Package 'lemon' is installed
  if (cap_axis_lines == TRUE) {
    if (!"lemon" %in% installed_pkgs) {
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
    ggplot2::theme_classic(base_size = base_size) + ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = legend_position,
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(
        t = axis_title_margin_size)),
      axis.title.y = ggplot2::element_text(
        angle = 0, vjust = y_axis_title_vjust,
        margin = ggplot2::margin(r = axis_title_margin_size)),
      axis.title = ggplot2::element_text(
        face = "bold", color = "black", size = axis_title_font_size),
      axis.text = ggplot2::element_text(
        face = "bold", color= "black", size = axis_tick_font_size),
      axis.ticks = ggplot2::element_line(colour = "black"),
      legend.title = ggplot2::element_text(
        face = "bold", color = "black", size = axis_title_font_size),
      legend.text = ggplot2::element_text(
        face = "bold", color= "black", size = axis_tick_font_size),
      legend.spacing.y = ggplot2::unit(legend_spacing_y, "cm"),
      legend.key.size = ggplot2::unit(legend_key_size, "lines")
    )
  # output so far
  output <- theme_object
  # cap the axis lines to the outer ticks
  if (cap_axis_lines == TRUE) {
    # https://github.com/stefanedwards/lemon/issues/26
    # refer to README in the package 'lemon'
    theme_object <- theme_object +
      ggplot2::theme(
        panel.border = ggplot2::element_blank(),
        axis.line = ggplot2::element_line())
    output <- list(
      theme_object,
      coord_cap_fn_from_lemon(left = "both", bottom = "both"))
  }
  return(output)
}
