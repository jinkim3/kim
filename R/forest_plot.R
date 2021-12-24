#' Forest plot
#'
#' Create a forest plot using outputs from 'metafor' package
#'
#' @param estimates default = NULL
#' @param estimate_ci_ll default = NULL
#' @param estimate_ci_ul default = NULL
#' @param point_size_range default = c(2, 10)
#' @param error_bar_size default = 1
#' @param error_bar_tip_height default = 0.3
#' @param weights default = NULL
#' @param diamond_x default = NULL
#' @param diamond_ci_ll default = NULL
#' @param diamond_ci_ul default = NULL
#' @param diamond_height default = 1.2
#' @param diamond_gap_height default = 0.3
#' @param diamond_1_tip_at_top_y default = -0.5
#' @param diamond_colors default = "black"
#' @param study_labels default = NULL
#' @param diamond_labels default = NULL
#' @param diamond_label_size default = 6
#' @param diamond_label_hjust default = 0
#' @param diamond_label_fontface default = "bold"
#' @param diamond_estimate_label_hjust default = 0
#' @param diamond_estimate_label_size default = 6
#' @param diamond_estimate_label_fontface default = "bold"
#' @param round_estimates default = 2
#' @param x_axis_title default = "Observed Outcome"
#' @param vline_size default = 1
#' @param vline_intercept default = 0
#' @param vline_type default = "dotted"
#' @param study_label_hjust default = 0
#' @param study_label_begin_x default = NULL
#' @param study_label_begin_x_perc default = 60
#' @param study_label_size default = 6
#' @param study_label_fontface default = "plain"
#' @param estimate_label_begin_x default = NULL
#' @param estimate_label_begin_x_perc default = 25
#' @param estimate_label_hjust default = 0
#' @param estimate_label_size default = 6
#' @param estimate_label_fontface default = "plain"
#' @param x_axis_tick_marks default = NULL
#' @param legend_position default = "none"
#' @param plot_margin default = NULL
#' @examples
#' \donttest{
#' forest_plot(
#' estimates = c(2, 3, 4),
#' estimate_ci_ll = c(1, 2, 3),
#' estimate_ci_ul = c(3, 4, 6),
#' weights = 1:3,
#' diamond_x = 2,
#' diamond_labels = "RE",
#' diamond_ci_ll = 1.8,
#' diamond_ci_ul = 2.2,
#' estimate_label_begin_x_perc = 40,
#' x_axis_tick_marks = seq(-2, 6, 2))
#' }
#' @export
forest_plot <- function(
  estimates = NULL,
  estimate_ci_ll = NULL,
  estimate_ci_ul = NULL,
  point_size_range = c(2, 10),
  error_bar_size = 1,
  error_bar_tip_height = 0.3,
  weights = NULL,
  diamond_x = NULL,
  diamond_ci_ll = NULL,
  diamond_ci_ul = NULL,
  diamond_height = 1.2,
  diamond_gap_height = 0.3,
  diamond_1_tip_at_top_y = -0.5,
  diamond_colors = "black",
  study_labels = NULL,
  diamond_labels = NULL,
  diamond_label_size = 6,
  diamond_label_hjust = 0,
  diamond_label_fontface = "bold",
  diamond_estimate_label_hjust = 0,
  diamond_estimate_label_size = 6,
  diamond_estimate_label_fontface = "bold",
  round_estimates = 2,
  x_axis_title = "Observed Outcome",
  vline_size = 1,
  vline_intercept = 0,
  vline_type = "dotted",
  study_label_hjust = 0,
  study_label_begin_x = NULL,
  study_label_begin_x_perc = 60,
  study_label_size = 6,
  study_label_fontface = "plain",
  estimate_label_begin_x = NULL,
  estimate_label_begin_x_perc = 25,
  estimate_label_hjust = 0,
  estimate_label_size = 6,
  estimate_label_fontface = "plain",
  x_axis_tick_marks = NULL,
  legend_position = "none",
  plot_margin = NULL
) {
  # bind the vars locally to the function
  study_order <- study_label <- estimate_label <- x <- y <- NULL
  diamond_label <- diamond_label_y <- diamond_estimate_label <- NULL
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
  # data table of values for plotting
  dt1 <- data.table::data.table(
    estimates, estimate_ci_ll, estimate_ci_ul)
  # reverse study order
  data.table::set(
    dt1, j = "study_order", value = rev(seq_along(estimates)))
  # add study labels and weights
  data.table::set(dt1, j = "weight", value = weights)
  if (is.null(study_labels)) {
    study_labels <- seq_along(estimates)
  }
  data.table::set(dt1, j = "study_label", value = study_labels)
  data.table::set(
    dt1, j = "estimate_label", value = paste0(
      sprintf("%.2f", round(estimates, round_estimates)), " [",
      sprintf("%.2f", round(estimate_ci_ll, round_estimates)), ", ",
      sprintf("%.2f", round(estimate_ci_ul, round_estimates)), "]"))
  # number of diamonds
  number_of_diamonds <- length(diamond_x)
  # add labels for diamonds
  # label for the diamond
  if (is.null(diamond_labels)) {
    diamond_labels <- rep("Unknown Type of Model", number_of_diamonds)
  }
  dt2 <- data.table::data.table(diamond_label = diamond_labels)
  data.table::set(
    dt2, j = "diamond_estimate_label", value = paste0(
      sprintf("%.2f", round(diamond_x, round_estimates)), " [",
      sprintf("%.2f", round(diamond_ci_ll, round_estimates)), ", ",
      sprintf("%.2f", round(diamond_ci_ul, round_estimates)), "]"))
  # parameters for plotting
  x_values <- unlist(dt1[, c(
    "estimates", "estimate_ci_ll", "estimate_ci_ul"), with = FALSE])
  x_min <- min(x_values, na.rm = TRUE)
  x_max <- max(x_values, na.rm = TRUE)
  x_range <- x_max - x_min
  # study label area begin
  if (is.null(study_label_begin_x)) {
    study_label_begin_x <- x_min - x_range *
      study_label_begin_x_perc / 100
  }
  # estimate label area begin
  if (is.null(estimate_label_begin_x)) {
    estimate_label_begin_x <- x_min - x_range *
      estimate_label_begin_x_perc / 100
  }
  # coordinates for drawing diamonds
  # x coordinates
  diamond_tip_at_top_x <- diamond_tip_at_bottom_x <- diamond_x
  diamond_left_tip_x <- diamond_ci_ll
  diamond_right_tip_x <- diamond_ci_ul
  # y coordinates
  diamond_tip_at_top_y <- seq(
    from = diamond_1_tip_at_top_y,
    to = diamond_1_tip_at_top_y - (number_of_diamonds - 1) * (
      diamond_height + diamond_gap_height),
    by = -(diamond_height + diamond_gap_height))
  diamond_tip_at_bottom_y <- diamond_tip_at_top_y - diamond_height
  diamond_left_tip_y <- diamond_right_tip_y <- diamond_tip_at_top_y -
    diamond_height / 2
  # update the diamond label dt
  data.table::set(
    dt2, j = "diamond_label_y", value = diamond_left_tip_y)
  # diamond colors
  if (number_of_diamonds > length(diamond_colors)) {
    if (length(diamond_colors) == 1) {
      diamond_colors <- rep(diamond_colors, number_of_diamonds)
    } else {
      stop(paste0(
        "The number of diamond colors is less than ",
        "the number of diamonds to draw."))
    }
  }
  # begin plotting
  g1 <- ggplot2::ggplot(
    dt1, ggplot2::aes(x = estimates, y = study_order, size = weights))
  # add a vertical line
  for (i in seq_along(vline_intercept)) {
    g1 <- g1 + ggplot2::geom_segment(
      x = vline_intercept[i],
      y = diamond_tip_at_bottom_y - diamond_gap_height * 2,
      xend = vline_intercept[i],
      yend = dt1[, max(study_order)] + 0.5,
      size = vline_size,
      linetype = vline_type)
  }
  # estimate from each study as square
  g1 <- g1 + ggplot2::geom_point(shape = 15)
  if (!is.null(point_size_range)) {
    g1 <- g1 + ggplot2::scale_size(range = point_size_range)
  }
  # add error bars
  g1 <- g1 + ggplot2::geom_errorbarh(ggplot2::aes(
    xmin = estimate_ci_ll, xmax = estimate_ci_ul, y = study_order),
    size = error_bar_size,
    height = error_bar_tip_height)
  # add study labels
  g1 <- g1 + ggplot2::geom_text(
    data = dt1, ggplot2::aes(
      label = study_label,
      x = study_label_begin_x,
      y = study_order),
    hjust = study_label_hjust,
    size = study_label_size,
    fontface = study_label_fontface)
  # add estimate labels
  g1 <- g1 + ggplot2::geom_text(
    data = dt1, ggplot2::aes(
      label = estimate_label,
      x = estimate_label_begin_x,
      y = study_order),
    hjust = estimate_label_hjust,
    size = estimate_label_size,
    fontface = estimate_label_fontface)
  # add a horizontal line
  g1 <- g1 + ggplot2::geom_hline(yintercept = 0)
  # add a diamond
  for (i in seq_len(number_of_diamonds)) {
    temp_dt <- data.table::data.table(
      x = c(
        diamond_tip_at_top_x[i],
        diamond_right_tip_x[i],
        diamond_tip_at_bottom_x[i],
        diamond_left_tip_x[i]),
      y = c(
        diamond_tip_at_top_y[i],
        diamond_right_tip_y[i],
        diamond_tip_at_bottom_y[i],
        diamond_left_tip_y[i]))
    g1 <- g1 + ggplot2::geom_polygon(
      data = temp_dt, ggplot2::aes(x = x, y = y),
      fill = diamond_colors[i],
      inherit.aes = FALSE)
  }
  # add the diamond label
  g1 <- g1 + ggplot2::geom_text(
    data = dt2, ggplot2::aes(
      label = diamond_label,
      x = study_label_begin_x,
      y = diamond_label_y),
    hjust = diamond_label_hjust,
    size = diamond_label_size,
    fontface = diamond_label_fontface)
  # add the diamond estimates
  g1 <- g1 + ggplot2::geom_text(
    data = dt2, ggplot2::aes(
      label = diamond_estimate_label,
      x = estimate_label_begin_x,
      y = diamond_label_y),
    hjust = diamond_estimate_label_hjust,
    size = diamond_estimate_label_size,
    fontface = diamond_estimate_label_fontface)
  # apply theme
  g1 <- g1 + ggplot2::theme_classic()
  # remove the axes
  g1 <- g1 + ggplot2::theme(
    axis.line = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    legend.position = legend_position)
  g1 <- g1 + ggplot2::coord_cartesian(clip = "off")
  # edit the x axis
  if (!is.null(x_axis_tick_marks)) {
    # the horizontal segment of the bracket
    g1 <- g1 + ggplot2::geom_segment(
      x = min(x_axis_tick_marks),
      y = diamond_tip_at_bottom_y - diamond_gap_height * 2,
      xend = max(x_axis_tick_marks),
      yend = diamond_tip_at_bottom_y - diamond_gap_height * 2,
      color = "black",
      size = 0.5,
      inherit.aes = FALSE)
    # the vertical segment at the bottom of the bracket
    tick_mark_bottom_tip_y <- diamond_tip_at_bottom_y -
      diamond_gap_height * 2 - nrow(dt1) / 30
    for (i in seq_along(x_axis_tick_marks)) {
      g1 <- g1 + ggplot2::geom_segment(
        x = x_axis_tick_marks[i],
        y = diamond_tip_at_bottom_y - diamond_gap_height * 2,
        xend = x_axis_tick_marks[i],
        yend = tick_mark_bottom_tip_y,
        color = "black",
        size = 0.5,
        inherit.aes = FALSE)
      g1 <- g1 + ggplot2::annotate(
        geom = "text",
        x = x_axis_tick_marks[i],
        y = tick_mark_bottom_tip_y,
        label = x_axis_tick_marks[i],
        vjust = 2,
        size = 6, fontface = "plain")
    }
  }
  # plot margins
  if (is.null(plot_margin)) {
    plot_margin = ggplot2::unit(c(7, 7, 40, 7), "pt")
  }
  g1 <- g1 + ggplot2::theme(plot.margin = plot_margin)
  # output
  return(g1)
}
