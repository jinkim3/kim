#' Draw a bracket on a ggplot
#'
#' Draw a square bracket with a label on a ggplot
#'
#' @param xmin xmin
#' @param xmax xmax
#' @param ymin ymin
#' @param ymax ymax
#' @param vertical vertical
#' @param horizontal horizontal
#' @param open open
#' @param bracket_shape bracket_shape
#' @param thickness thickness
#' @param bracket_color bracket_color
#' @param label label
#' @param label_hjust label_hjust
#' @param label_vjust label_vjust
#' @param label_font_size label_font_size
#' @param label_font_face label_font_face
#' @param label_color label_font_face
#' @param label_parse label_parse
#' @return a ggplot object; there will be no meaningful output from
#' this function. Instead, this function should be used with another
#' ggplot object
#' @examples
#' \donttest{
#' prep(ggplot2)
#' ggplot(mtcars, aes(x = cyl, y = mpg)) + geom_point() +
#' bracket(6.1, 6.2, 17, 22, bracket_shape = "]", label = "abc")
#' }
#' @export
bracket <- function(
  xmin = NULL,
  xmax = NULL,
  ymin = NULL,
  ymax = NULL,
  vertical = NULL,
  horizontal = NULL,
  open = NULL,
  bracket_shape = NULL,
  thickness = 3,
  bracket_color = "black",
  label = NULL,
  label_hjust = NULL,
  label_vjust = NULL,
  label_font_size = 5,
  label_font_face = "bold",
  label_color = "black",
  label_parse = FALSE) {
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
  } else {
    # proceed if Package 'ggplot2' is already installed
    geom_segment_fn <- utils::getFromNamespace("geom_segment", "ggplot2")
    annotate_fn <- utils::getFromNamespace("annotate", "ggplot2")
    aes_fn <- utils::getFromNamespace("aes", "ggplot2")
  }
  # bracket shape
  if (!is.null(bracket_shape)) {
    if (bracket_shape == "]") {
      vertical <- TRUE
      horizontal <- FALSE
      open <- "left"
    } else if (bracket_shape == "[") {
      vertical <- TRUE
      horizontal <- FALSE
      open <- "right"
    } else if (bracket_shape %in% c("u", "U")) {
      vertical <- FALSE
      horizontal <- TRUE
      open <- "up"
    } else if (bracket_shape %in% c("inverted_u")) {
      vertical <- FALSE
      horizontal <- TRUE
      open <- "down"
    }
  }
  # check the vertical and horizontal arguments
  if (vertical == TRUE & horizontal == TRUE) {
    stop(paste0(
      'The arguments "vertical" and "horizontal" cannot both set to',
      " be TRUE. Please set only ONE of them to be TRUE."))
  }
  # check the open argument
  if (!open %in% c("left", "right", "up", "down")) {
    stop(paste0(
      'The input for the argument "open" must be one of the following:',
      ' "left", "right", "up", "down"'))
  }
  # vertical vs horizontal
  if (vertical == TRUE & open == "left") {
    # segments
    bracket_element_list <- list(
      geom_segment_fn(
        x = xmax, xend = xmax, y = ymin, yend = ymax,
        size = thickness, color = bracket_color),
      geom_segment_fn(
        x = xmin, xend = xmax, y = ymin, yend = ymin,
        size = thickness, color = bracket_color),
      geom_segment_fn(
        x = xmin, xend = xmax, y = ymax, yend = ymax,
        size = thickness, color = bracket_color))
    # label hjust and vjust
    if (is.null(label_hjust)) {
      label_hjust <- -1
    }
    if (is.null(label_vjust)) {
      label_vjust <- 0.5
    }
    # label position
    annotate_x <- xmax
    annotate_y <- mean(c(ymin, ymax))
  } else if (vertical == TRUE & open == "right") {
    # segments
    bracket_element_list <- list(
      geom_segment_fn(
        x = xmin, xend = xmin, y = ymin, yend = ymax,
        size = thickness, color = bracket_color),
      geom_segment_fn(
        x = xmin, xend = xmax, y = ymin, yend = ymin,
        size = thickness, color = bracket_color),
      geom_segment_fn(
        x = xmin, xend = xmax, y = ymax, yend = ymax,
        size = thickness, color = bracket_color))
    # label hjust and vjust
    if (is.null(label_hjust)) {
      label_hjust <- 2
    }
    if (is.null(label_vjust)) {
      label_vjust <- 0.5
    }
    # label position
    annotate_x <- xmin
    annotate_y <- mean(c(ymin, ymax))
  } else if (horizontal == TRUE & open == "up") {
    # segments
    bracket_element_list <- list(
      geom_segment_fn(
        x = xmin, xend = xmax, y = ymin, yend = ymin,
        size = thickness, color = bracket_color),
      geom_segment_fn(
        x = xmin, xend = xmin, y = ymin, yend = ymax,
        size = thickness, color = bracket_color),
      geom_segment_fn(
        x = xmax, xend = xmax, y = ymin, yend = ymax,
        size = thickness, color = bracket_color))
    # label hjust and vjust
    if (is.null(label_hjust)) {
      label_hjust <- 0.5
    }
    if (is.null(label_vjust)) {
      label_vjust <- 2
    }
    # label position
    annotate_x <- mean(c(xmin, xmax))
    annotate_y <- ymin
  } else if (horizontal == TRUE & open == "down") {
    # segments
    bracket_element_list <- list(
      geom_segment_fn(
        x = xmin, xend = xmax, y = ymax, yend = ymax,
        size = thickness, color = bracket_color),
      geom_segment_fn(
        x = xmin, xend = xmin, y = ymin, yend = ymax,
        size = thickness, color = bracket_color),
      geom_segment_fn(
        x = xmax, xend = xmax, y = ymin, yend = ymax,
        size = thickness, color = bracket_color))
    # label hjust and vjust
    if (is.null(label_hjust)) {
      label_hjust <- 0.5
    }
    if (is.null(label_vjust)) {
      label_vjust <- -1
    }
    # label position
    annotate_x <- mean(c(xmin, xmax))
    annotate_y <- ymax
  }
  # annotation
  annotation <- annotate_fn(
    geom = "text", x = annotate_x, y = annotate_y,
    label = label, hjust = label_hjust, vjust = label_vjust,
    size = label_font_size, fontface = label_font_face,
    parse = label_parse)
  # output
  output <- c(bracket_element_list, list(annotation))
  return(output)
}
