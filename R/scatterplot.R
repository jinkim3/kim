#' Scatterplot
#'
#' Creates a scatter plot and calculates a correlation between two variables.
#'
#' If a weighted correlation is to be calculated, the following package(s)
#' must be installed prior to running the function:
#' Package 'weights' v1.0 (or possibly a higher version) by
#' John Pasek (2018),
#' <https://cran.r-project.org/package=weights>
#'
#' @param data a data object (a data frame or a data.table)
#' @param x_var_name name of the variable that will go on the x axis
#' @param y_var_name name of the variable that will go on the y axis
#' @param dot_label_var_name name of the variable that will be used to
#' label individual observations
#' @param weight_var_name name of the variable by which to weight
#' the individual observations for calculating correlation and plotting
#' the line of fit
#' @param alpha opacity of the dots (0 = completely transparent,
#' 1 = completely opaque)
#' @param annotate_stats if \code{TRUE}, the correlation and p-value will
#' be annotated at the top of the plot (default = TRUE)
#' @param annotate_y_pos position of the annotated stats, expressed
#' as a percentage of the range of y values by which the annotated
#' stats will be placed above the maximum value of y in the data set
#' (default = 5). If \code{annotate_y_pos = 5}, and the minimum and
#' maximum y values in the data set are 0 and 100, respectively,
#' the annotated stats will be placed at 5% of the y range (100 - 0)
#' above the maximum y value, y = 0.05 * (100 - 0) + 100 = 105.
#' @param annotated_stats_color color of the annotated stats
#' (default = "green4").
#' @param annotated_stats_font_size font size of the annotated stats
#' (default = 6).
#' @param annotated_stats_font_face font face of the annotated stats
#' (default = "bold").
#' @param line_of_fit_type if \code{line_of_fit_type = "lm"}, a regression
#' line will be fit; if \code{line_of_fit_type = "loess"}, a local
#' regression line will be fit; if \code{line_of_fit_type = "none"},
#' no line will be fit
#' @param ci_for_line_of_fit if \code{ci_for_line_of_fit = TRUE},
#' confidence interval for the line of fit will be shaded
#' @param line_of_fit_color color of the line of fit (default = "blue")
#' @param line_of_fit_thickness thickness of the line of fit (default = 1)
#' @param dot_color color of the dots (default = "black")
#' @param x_axis_label alternative label for the x axis
#' @param y_axis_label alternative label for the y axis
#' @param dot_size size of the dots on the plot (default = 2)
#' @param dot_label_size size for dots' labels on the plot. If no
#' input is entered for this argument, it will be set as
#' \code{dot_label_size = 5} by default. If the plot is to be
#' weighted by some variable, this argument will be ignored, and
#' dot sizes will be determined by the argument \code{dot_size_range}
#' @param dot_size_range minimum and maximum size for dots
#' on the plot when they are weighted
#' @param jitter_x_y_percent horizontally and vertically jitter dots
#' by a percentage of the range of x and y values.
#' @param jitter_x_percent horizontally jitter dots by a percentage of the
#' range of x values.
#' @param jitter_y_percent vertically jitter dots by a percentage of the
#' range of y values
#' @param cap_axis_lines logical. Should the axis lines be capped at the
#' outer tick marks? (default = TRUE)
#' @param color_dots_by name of the variable that will determine
#' colors of the dots
#' @param png_name name of the PNG file to be saved. By default, the name
#' will be "scatterplot_" followed by a timestamp of the
#' current time.
#' The timestamp will be in the format, jan_01_2021_1300_10_000001,
#' where "jan_01_2021" would indicate January 01, 2021;
#' 1300 would indicate 13:00 (i.e., 1 PM); and 10_000001 would
#' indicate 10.000001 seconds after the hour.
#' @param save_as_png if \code{save = TRUE}, the plot will be saved
#' as a PNG file.
#' @return the output will be a scatter plot, a ggplot object.
#' @examples
#' \dontrun{
#' scatterplot(data = mtcars, x_var_name = "wt", y_var_name = "mpg")
#' scatterplot(
#'   data = mtcars, x_var_name = "wt", y_var_name = "mpg",
#'   dot_label_var_name = "hp", weight_var_name = "drat",
#'   annotate_stats = TRUE)
#' scatterplot(
#'   data = mtcars, x_var_name = "wt", y_var_name = "mpg",
#'   dot_label_var_name = "hp", weight_var_name = "cyl",
#'   dot_label_size = 7, annotate_stats = TRUE)
#' }
#' @export
#' @import data.table
scatterplot <- function(
  data = NULL,
  x_var_name = NULL,
  y_var_name = NULL,
  dot_label_var_name = NULL,
  weight_var_name = NULL,
  alpha = 1,
  annotate_stats = TRUE,
  annotate_y_pos = 5,
  annotated_stats_color = "green4",
  annotated_stats_font_size = 6,
  annotated_stats_font_face = "bold",
  line_of_fit_type = "lm",
  ci_for_line_of_fit = FALSE,
  line_of_fit_color = "blue",
  line_of_fit_thickness = 1,
  dot_color = "black",
  x_axis_label = NULL,
  y_axis_label = NULL,
  dot_size = 2,
  dot_label_size = NULL,
  dot_size_range = c(3, 12),
  jitter_x_percent = 0,
  jitter_y_percent = 0,
  jitter_x_y_percent = 0,
  cap_axis_lines = TRUE,
  color_dots_by = NULL) {
  # bind the vars locally to the function
  x <- y <- NULL
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
  # weighted correlation
  if (!is.null(weight_var_name)) {
    # check if weights package is installed
    if (!"weights" %in% installed_pkgs) {
      message(paste0(
        "To calculate weighted correlation(s), Package 'weights' must ",
        "be installed.\nTo install Package 'weights', type ",
        "'kim::prep(weights)'",
        "\n\nAlternatively, to install all packages (dependencies) required ",
        "for all\nfunctions in Package 'kim', type ",
        "'kim::install_all_dependencies()'"))
      return()
    } else {
      # proceed if weights package is already installed
      wtd_cor_function <- utils::getFromNamespace("wtd.cor", "weights")
    }
  }
  # create a temporary dataset
  dt01 <- data.table::data.table(
    x = data[[x_var_name]],
    y = data[[y_var_name]])
  # add the column for colors
  if (!is.null(color_dots_by)) {
    data.table::set(
      dt01, j = "color", value = data[[color_dots_by]])
  }
  # add the point label or weight column
  if (!is.null(dot_label_var_name)) {
    data.table::set(
      dt01, j = "dot_labels", value = data[[dot_label_var_name]])
  }
  if (!is.null(weight_var_name)) {
    data.table::set(
      dt01, j = "weight", value = data[[weight_var_name]])
  }
  # remove na values
  num_of_na_rows <- sum(!stats::complete.cases(dt01))
  if (num_of_na_rows > 0) {
    dt02 <- stats::na.omit(dt01)
    message(paste0(
      num_of_na_rows,
      " rows were removed because of missing values."))
  } else {
    dt02 <- dt01
  }
  # ranges for x and y
  x_range <- max(dt02$x) - min(dt02$x)
  y_range <- max(dt02$y) - min(dt02$y)
  # start ggplot
  g1 <- ggplot2::ggplot(data = dt02, ggplot2::aes(
    x = x, y = y))
  # add the color
  if (!is.null(color_dots_by)) {
    g1 <- g1 + ggplot2::aes(color = dt02$color)
  }
  # add jitter
  if (jitter_x_y_percent > 0) {
    jitter_x_percent <- jitter_x_y_percent
    jitter_y_percent <- jitter_x_y_percent
  }
  if (jitter_x_percent > 0 | jitter_y_percent > 0) {
    pj <- ggplot2::position_jitter(
      width = jitter_x_percent / 100 * x_range,
      height = jitter_y_percent / 100 * y_range)
  }
  # add point labels or dots
  if (!is.null(dot_label_var_name)) {
    g1 <- g1 + ggplot2::aes(label = dt02$dot_labels)
    if (is.null(dot_label_size)) {
      # add jitter if necessary
      if (jitter_x_percent > 0 | jitter_y_percent > 0) {
        g1 <- g1 + ggplot2::geom_text(
          ggplot2::aes(label = dt02$dot_labels, fontface = "bold"),
          position = pj)
      } else {
        g1 <- g1 + ggplot2::geom_text(
          ggplot2::aes(label = dt02$dot_labels, fontface = "bold"))
      }
    } else {
      # add jitter if necessary
      if (jitter_x_percent > 0 | jitter_y_percent > 0) {
        g1 <- g1 + ggplot2::geom_text(
          ggplot2::aes(label = dt02$dot_labels, fontface = "bold"),
          position = pj,
          size = dot_label_size)
      } else {
        g1 <- g1 + ggplot2::geom_text(
          ggplot2::aes(label = dt02$dot_labels, fontface = "bold"),
          size = dot_label_size)
      }
    }
  } else {
    # add jitter if necessary
    if (jitter_x_percent > 0 | jitter_y_percent > 0) {
      g1 <- g1 + ggplot2::geom_point(
        alpha = alpha, size = dot_size, position = pj,
        color = dot_color)
    } else {
      g1 <- g1 + ggplot2::geom_point(
        alpha = alpha, size = dot_size,
        color = dot_color)
    }
  }
  # scale points
  if (!is.null(weight_var_name)) {
    g1 <- g1 + ggplot2::aes(size = dt02$weight)
    g1 <- g1 + ggplot2::scale_size(
      range = dot_size_range, guide = FALSE)
  }
  # weighted least squares line
  if (line_of_fit_type %in% c("lm", "loess")) {
    # weighted or not
    if (!is.null(weight_var_name)) {
      g1 <- g1 + ggplot2::geom_smooth(
        formula = y ~ x,
        method = line_of_fit_type,
        mapping = ggplot2::aes(weight = dt02$weight),
        color = line_of_fit_color,
        size = line_of_fit_thickness,
        se = ci_for_line_of_fit)
    } else {
      g1 <- g1 + ggplot2::geom_smooth(
        formula = y ~ x,
        method = line_of_fit_type,
        color = line_of_fit_color,
        size = line_of_fit_thickness,
        se = ci_for_line_of_fit)
    }
  }
  # correlation
  cor_test <- stats::cor.test(dt02[["x"]], dt02[["y"]])
  cor_test_df <- cor_test[["parameter"]][["df"]]
  # weighted or not
  if (!is.null(weight_var_name)) {
    cor_test <- wtd_cor_function(
      x = dt02$x, y = dt02$y,
      weight = dt02$weight)
    cor_test_r <- cor_test[1, "correlation"]
    cor_test_p_value <- cor_test[1, "p.value"]
    weighted_r_text <- "weighted"
  } else {
    # correlation
    cor_test_r <- cor_test[["estimate"]]
    cor_test_p_value <- cor_test[["p.value"]]
    weighted_r_text <- ""
  }
  # nice p value
  cor_test_p_value_text <- kim::pretty_round_p_value(
    cor_test_p_value, include_p_equals = TRUE)
  # annotate stats
  if (annotate_stats == TRUE) {
    annotation_01 <-
      as.character(as.expression(substitute(
        t06 * italic(t01)(t02) == t03 * t04 * italic(p) * t05,
        list(
          t01 = " r",
          t02 = cor_test_df,
          t03 = sub(
            "^(-?)0.", "\\1.",
            sprintf(paste0("%.", 2, "f"), cor_test_r)
          ),
          t04 = ", ",
          t05 = gsub("p", "", cor_test_p_value_text),
          t06 = weighted_r_text
        )
      )))
    g1 <- g1 + ggplot2::annotate(
      geom = "text",
      x = min(dt02$x) + x_range / 2,
      y = max(dt02$y) + y_range * annotate_y_pos / 100,
      color = annotated_stats_color,
      label = annotation_01, parse = TRUE,
      hjust = 0.5, vjust = 0.5,
      size = annotated_stats_font_size,
      fontface = annotated_stats_font_face)
  }
  # axis labels
  if (is.null(x_axis_label)) {
    x_axis_label <- x_var_name
  }
  if (is.null(y_axis_label)) {
    y_axis_label <- y_var_name
  }
  g1 <- g1 + ggplot2::xlab(x_axis_label)
  g1 <- g1 + ggplot2::ylab(y_axis_label)
  # plot theme
  g1 <- g1 + kim::theme_kim(cap_axis_lines = cap_axis_lines)
  print(g1)
  # save as png
  if (save_as_png == TRUE & is.null(png_name)) {
    # default file name
    if (is.null(png_name)) {
      ts <- tolower(
        gsub("\\.", "_", format(Sys.time(), "_%b_%d_%Y_%H%M_%OS6")))
      png_name <- paste0("scatterplot_", ts)
    }
  }
  if (!is.null(png_name)) {
    kim::ggsave_quick(g1, png_name, width = width, height = height)
  }
  # return the ggplot
  invisible(g1)
}
