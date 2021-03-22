#' Floodlight 2 by Continuous
#'
#' Conduct a floodlight analysis for 2 x Continuous design
#'
#' @param data a data object (a data frame or a data.table)
#' @param iv_name name of the binary independent variable
#' @param dv_name name of the dependent variable
#' @param mod_name name of the continuous moderator variable
#' @param interaction_p_include logical. Should the plot include a
#' p-value for the interaction term?
#' @param iv_level_order order of levels in the independent
#' variable for legend. By default, it will be set as levels of the
#' independent variable ordered using R's base function \code{sort}.
#' @param output type of output (default = "reg_lines_plot").
#' @param jitter_x_percent horizontally jitter dots by a percentage of the
#' range of x values
#' @param jitter_y_percent vertically jitter dots by a percentage of the
#' range of y values#'
#' @param dot_alpha opacity of the dots (0 = completely transparent,
#' 1 = completely opaque). By default, \code{dot_alpha = 0.5}
#' @param dot_size size of the dots (default = 4)
#' @param legend_position position of the legend (default = "right").
#' If \code{legend_position = "none"}, the legend will be removed.
#' @param reg_line_types types of the regression lines for the two levels
#' of the independent variable.
#' By default, \code{reg_line_types = c("solid", "dashed")}
#' @param jn_line_types types of the lines for Johnson-Neyman points.
#' By default, \code{jn_line_types = c("solid", "solid")}
#' @param sig_region_color color of the significant region, i.e., range(s)
#' of the moderator variable for which simple effect of the independent
#' variable on the dependent variable is statistically significant.
#' @param sig_region_alpha opacity for \code{sig_region_color}.
#' (0 = completely transparent, 1 = completely opaque).
#' By default, \code{sig_region_alpha = 0.08}
#' @param nonsig_region_color color of the non-significant region,
#' i.e., range(s) of the moderator variable for which simple effect of
#' the independent variable on the dependent variable is not
#' statistically significant.
#' @param nonsig_region_alpha opacity for \code{nonsig_region_color}.
#' (0 = completely transparent, 1 = completely opaque).
#' By default, \code{nonsig_region_alpha = 0.08}
#' @param x_axis_title title of the x axis. By default, it will be set
#' as input for \code{mod_name}. If \code{x_axis_title = FALSE}, it will
#' be removed.
#' @param y_axis_title title of the y axis. By default, it will be set
#' as input for \code{dv_name}. If \code{y_axis_title = FALSE}, it will
#' be removed.
#' @param legend_title title of the legend. By default, it will be set
#' as input for \code{iv_name}. If \code{legend_title = FALSE}, it will
#' be removed.
#' @param round_decimals_int_p_value To how many digits after the
#' decimal point should the p value for the interaction term be
#' rounded? (default = 3)
#' @examples
#' floodlight_2_by_continuous(
#' data = mtcars,
#' iv_name = "am",
#' dv_name = "mpg",
#' mod_name = "qsec")
#' @export
#' @import data.table ggplot2 interactions
floodlight_2_by_continuous <- function(
  data = NULL,
  iv_name = NULL,
  dv_name = NULL,
  mod_name = NULL,
  interaction_p_include = TRUE,
  iv_level_order = NULL,
  output = "reg_lines_plot",
  jitter_x_percent = 0,
  jitter_y_percent = 0,
  dot_alpha = 0.5,
  dot_size = 4,
  legend_position = "right",
  reg_line_types = c("solid", "dashed"),
  jn_line_types = c("solid", "solid"),
  sig_region_color = "green",
  sig_region_alpha = 0.08,
  nonsig_region_color = "gray",
  nonsig_region_alpha = 0.08,
  x_axis_title = NULL,
  y_axis_title = NULL,
  legend_title = NULL,
  round_decimals_int_p_value = 3
) {
  # bind the vars locally to the function
  dv <- iv_binary <- iv_factor <- mod <- NULL
  # convert to data.table
  dt_1 <- data.table::setDT(data.table::copy(data))
  # remove rows with na
  dt_1 <- stats::na.omit(dt_1[, c(iv_name, dv_name, mod_name), with = F])
  # unique values in iv
  iv_unique_values <- sort(unique(dt_1[, get(iv_name)]))
  iv_unique_values_character <- as.character(iv_unique_values)
  # check if iv is binary
  num_of_levels_in_iv <- length(iv_unique_values)
  if (num_of_levels_in_iv != 2) {
    stop(paste0(
      "The independent variable has ", num_of_levels_in_iv,
      " levels.\n",
      "The current version of the function can only handle",
      "an independent variable with exactly two levels."))
  }
  # set the order of levels in iv
  if (is.null(iv_level_order)) {
    iv_level_1 <- iv_unique_values[1]
    iv_level_2 <- iv_unique_values[2]
  } else {
    # check if the iv levels match
    iv_level_order_character <- as.character(iv_level_order)
    if (!identical(iv_unique_values_character,
                   iv_level_order_character)) {
      stop(paste0(
        "\nThe levels of independent variables do not match:\n",
        "iv_level_order input: ",
        paste0(iv_level_order_character, collapse = ", "),
        "\nLevels of IV in the data set: ",
        paste0(iv_unique_values_character, collapse = ", ")))
    }
    iv_level_1 <- iv_level_order[1]
    iv_level_2 <- iv_level_order[2]
  }
  # copy data table
  dt_2 <- copy(dt_1)
  # add binary variable
  dt_2[, iv_binary := fcase(
    get(iv_name) == iv_level_1, 0,
    get(iv_name) == iv_level_2, 1)]
  # add a factor
  dt_2[, paste0(iv_name, "_factor") := factor(
    iv_binary,
    levels = 0:1,
    labels = c(as.character(iv_level_1), as.character(iv_level_2)))]
  names(dt_2) <- c("iv", "dv", "mod", "iv_binary", "iv_factor")
  # jn points
  johnson_neyman_result <- interactions::johnson_neyman(
    stats::lm(dv ~ iv_binary * mod, data = dt_2),
    pred = iv_binary,
    modx = mod)
  jn_points <- johnson_neyman_result[["bounds"]]
  # plot simple effects at values of moderator
  if (output == "simple_effects_plot") {
    g1 <- johnson_neyman_result[["plot"]]
    g1 <- g1 + ggtitle(paste0(
      "Johnson-Neyman Plot - ",
      "R Package 'interactions', Jacob A. Long (2020)"))
    g1 <- g1 + xlab(mod_name)
    g1 <- g1 + ylab(paste0("Slope of ", iv_name))
    return(g1)
  }
  # jitter
  x_range <- diff(range(dt_2[, mod]))
  y_range <- diff(range(dt_2[, dv]))
  # plot
  g1 <- ggplot(
    data = dt_2,
    aes(x = mod, y = dv,
        color = iv_factor,
        linetype = iv_factor))
  # plot points
  g1 <- g1 + geom_point(
    size = dot_size,
    alpha = dot_alpha,
    position = position_jitter(
      width = x_range * jitter_x_percent / 100,
      height = y_range * jitter_y_percent / 100))
  # plot regression lines
  g1 <- g1 + geom_smooth(
    formula = y ~ x,
    method = "lm", se = F)
  g1 <- g1 + scale_linetype_manual(
    values = reg_line_types)
  # include interaction p value
  if (interaction_p_include == TRUE) {
    lm_summary <- summary(stats::lm(dv ~ iv_binary * mod, data = dt_2))
    interaction_p_value <- kim::pretty_round_p_value(
      lm_summary[["coefficients"]]["iv_binary:mod", "Pr(>|t|)"],
      include_p_equals = TRUE,
      round_digits_after_decimal = round_decimals_int_p_value)
    interaction_p_value_text <- paste0(
      "Interaction ", interaction_p_value)
    # label jn points
    g1 <- g1 + annotate(
      geom = "text",
      x = min(dt_2[, mod]) + x_range * 0.5,
      y = Inf,
      label = interaction_p_value_text,
      hjust = 0.5, vjust = -3,
      fontface = "bold",
      color = "black",
      size = 6)
  }
  # positions of the lines marking johnson neyman points
  jn_line_pos <- jn_points
  mod_min_observed <- min(dt_2[, mod])
  mod_max_observed <- max(dt_2[, mod])
  if (jn_line_pos[["Lower"]] < mod_min_observed) {
    jn_line_pos[["Lower"]] <- -Inf
  }
  if (jn_line_pos[["Higher"]] > mod_max_observed) {
    jn_line_pos[["Higher"]] <- Inf
  }
  # apply the theme beforehand
  g1 <- g1 + kim::theme_kim(legend_position = legend_position)
  # allow labeling outside the plot area
  suppressMessages(g1 <- g1 + coord_cartesian(clip = "off"))
  g1 <- g1 + theme(plot.margin = unit(c(60, 7, 7, 7), "pt"))
  # if only one type is entered for jn line
  if (length(jn_line_types) == 1) {
    jn_line_types <- rep(jn_line_types, sum(is.finite(jn_line_pos)))
  }
  # add a vertical line and label for each jn point
  for(i in which(is.finite(jn_line_pos))) {
    # i <- 2
    # vertical line
    g1 <- g1 + geom_vline(
      xintercept = jn_line_pos[i],
      linetype = jn_line_types[i],
      size = 1)
    # label jn points
    g1 <- g1 + annotate(
      geom = "text",
      x = jn_line_pos[i],
      y = Inf,
      label = round(jn_line_pos[i], 2),
      hjust = 0.5, vjust = -0.5,
      fontface = "bold",
      color = "black",
      size = 6)
  }
  # shade
  sig_inside_vs_outside <- ifelse(
    attributes(johnson_neyman_result)$inside, "inside", "outside")
  # if sig area is outside
  if (sig_inside_vs_outside == "outside") {
    # sig area on the left
    g1 <- g1 + annotate(
      "rect", xmin = -Inf, xmax = jn_line_pos[["Lower"]],
      ymin = -Inf, ymax = Inf,
      alpha = sig_region_alpha, fill = sig_region_color)
    # nonsig area in the middle
    g1 <- g1 + annotate(
      "rect",
      xmin = jn_line_pos[["Lower"]],
      xmax = jn_line_pos[["Higher"]],
      ymin = -Inf, ymax = Inf,
      alpha = nonsig_region_alpha, fill = nonsig_region_color)
    # sig area on the right
    g1 <- g1 + annotate(
      "rect", xmin = jn_line_pos[["Higher"]], xmax = Inf,
      ymin = -Inf, ymax = Inf,
      alpha = sig_region_alpha, fill = sig_region_color)
  }
  # if sig area is inside
  if (sig_inside_vs_outside == "inside") {
    # nonsig area on the left
    g1 <- g1 + annotate(
      "rect", xmin = -Inf, xmax = jn_line_pos[["Lower"]],
      ymin = -Inf, ymax = Inf,
      alpha = nonsig_region_alpha, fill = nonsig_region_color)
    # sig area in the middle
    g1 <- g1 + annotate(
      "rect",
      xmin = jn_line_pos[["Lower"]],
      xmax = jn_line_pos[["Higher"]],
      ymin = -Inf, ymax = Inf,
      alpha = sig_region_alpha, fill = sig_region_color)
    # nonsig area on the right
    g1 <- g1 + annotate(
      "rect", xmin = jn_line_pos[["Higher"]], xmax = Inf,
      ymin = -Inf, ymax = Inf,
      alpha = nonsig_region_alpha, fill = nonsig_region_color)
  }
  # x axis title
  if (is.null(x_axis_title)) {
    g1 <- g1 + xlab(mod_name)
  } else {
    if (x_axis_title == FALSE) {
      g1 <- g1 + theme(axis.title.x = element_blank())
    } else {
      g1 <- g1 + xlab(x_axis_title)
    }
  }
  # y axis title
  if (is.null(y_axis_title)) {
    g1 <- g1 + ylab(dv_name)
  } else {
    if (y_axis_title == FALSE) {
      g1 <- g1 + theme(axis.title.y = element_blank())
    } else {
      g1 <- g1 + ylab(y_axis_title)
    }
  }
  # legend title
  if (is.null(legend_title)) {
    g1 <- g1 + labs(
      color = iv_name,
      linetype = iv_name)
  } else {
    if (legend_title == FALSE) {
      g1 <- g1 + theme(legend.title = element_blank())
    } else {
      g1 <- g1 + labs(
        color = legend_title,
        linetype = legend_title)
    }
  }
  print(g1)
  return(g1)
}
