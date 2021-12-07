#' Spotlight 2 by Continuous
#'
#' Conduct a spotlight analysis for a 2 x Continuous design.
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
#' @param output_type type of output (default = "plot").
#' @param colors set colors for the two levels of the independent variable
#' (default = \code{colors = c("red", "blue")}).
#' @param lines_connecting_means logical. Should lines connecting means
#' within each group be drawn? (default = TRUE)
#' @param line_size thickness of the lines connecting group means,
#' (default = 1)
#' @param dot_size size of the dots indicating group means (default = 3)
#' @param error_bar if \code{error_bar = "se"}; error bars will be +/-1
#' standard error, if \code{error_bar = "ci"} error bars will be a
#' confidence interval. By default, \code{error_bar = "ci"}.
#' @param error_bar_range width of the confidence interval
#' (default = 0.95 for a 95 percent confidence interval).
#' This argument will not apply when \code{error_bar = "se"}
#' @param error_bar_tip_width graphically, width of the segments
#' at the end of error bars (default = 0.13)
#' @param error_bar_thickness thickness of the error bars (default = 1)
#' @param position_dodge by how much should the group means and error bars
#' be horizontally offset from each other so as not to overlap?
#' (default = 0.13)
#' @param simple_effect_color color for the text indicating p-values
#' of simple effects (default = "black").
#' @param simple_effect_font_size font size of the text indicating
#' p-values of simple effects (default = 5).
#' @param interaction_p_value_font_size font size for the interaction
#' p value (default = 6)
#' @param x_axis_title title of the x axis. By default, it will be set
#' as input for \code{mod_name}. If \code{x_axis_title = FALSE}, it will
#' be removed.
#' @param y_axis_title title of the y axis. By default, it will be set
#' as input for \code{dv_name}. If \code{y_axis_title = FALSE}, it will
#' be removed.
#' @param legend_title title of the legend. By default, it will be set
#' as input for \code{iv_name}. If \code{legend_title = FALSE}, it will
#' be removed.
#' @param legend_position position of the legend (default = "right").
#' If \code{legend_position = "none"}, the legend will be removed.
#' @param y_axis_title_vjust position of the y axis title (default = 0.85).
#' If default is used, \code{y_axis_title_vjust = 0.85}, the y axis title
#' will be positioned at 85% of the way up from the bottom of the plot.
#' @param round_decimals_int_p_value To how many digits after the
#' decimal point should the p value for the interaction term be
#' rounded? (default = 3)
#' @param plot_margin margin for the plot
#' By default \code{plot_margin = ggplot2::unit(c(60, 7, 7, 7), "pt")}
#' @examples
#' \donttest{
#' spotlight_2_by_continuous(
#' data = mtcars,
#' iv_name = "am",
#' dv_name = "mpg",
#' mod_name = "qsec")
#' }
#' @export
#' @import data.table
spotlight_2_by_continuous <- function(
  data = NULL,
  iv_name = NULL,
  dv_name = NULL,
  mod_name = NULL,
  interaction_p_include = TRUE,
  iv_level_order = NULL,
  output_type = "plot",
  colors = c("red", "blue"),
  lines_connecting_means = TRUE,
  line_size = 1,
  dot_size = 3,
  error_bar = "ci",
  error_bar_range = 0.95,
  error_bar_tip_width = 0.13,
  error_bar_thickness = 1,
  position_dodge = 0.13,
  simple_effect_color = "black",
  simple_effect_font_size = 5,
  interaction_p_value_font_size = 6,
  x_axis_title = NULL,
  y_axis_title = NULL,
  legend_title = NULL,
  legend_position = "right",
  y_axis_title_vjust = 0.85,
  round_decimals_int_p_value = 3,
  plot_margin = ggplot2::unit(c(60, 7, 7, 7), "pt")
) {
  # for testing the code
  # library(data.table)
  # setDT(data)[, am := fcase(am == 0, "male", am == 1, "female")]
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
  # check whether all arguments had required inputs
  if (is.null(iv_name)) {
    stop("Please enter a variable name for the input 'iv_name'")
  }
  if (is.null(dv_name)) {
    stop("Please enter a variable name for the input 'dv_name'")
  }
  if (is.null(mod_name)) {
    stop("Please enter a variable name for the input 'mod_name'")
  }
  # bind the vars locally to the function
  dv <- iv_binary <- iv_factor <- mod <- NULL
  # convert to data.table
  dt <- data.table::setDT(data.table::copy(data))
  # remove rows with na
  dt <- stats::na.omit(dt[, c(iv_name, dv_name, mod_name), with = F])
  # unique values in iv
  iv_unique_values <- sort(unique(dt[, get(iv_name)]))
  iv_unique_values_character <- as.character(iv_unique_values)
  # check if iv is binary
  num_of_levels_in_iv <- length(iv_unique_values)
  if (num_of_levels_in_iv != 2) {
    stop(paste0(
      "The independent variable has ", num_of_levels_in_iv,
      " levels.\n",
      "The current version of the function can only handle",
      " an independent variable with exactly two levels."))
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
  # add binary variable
  dt[, iv_binary := data.table::fcase(
    get(iv_name) == iv_level_1, 0,
    get(iv_name) == iv_level_2, 1)]
  # add a factor
  dt[, paste0(iv_name, "_factor") := factor(
    iv_binary,
    levels = 0:1,
    labels = c(as.character(iv_level_1), as.character(iv_level_2)))]
  names(dt) <- c("iv", "dv", "mod", "iv_binary", "iv_factor")
  # focal values of the moderator
  mod_mean <- mean(dt[, mod])
  mod_sd <- sd(dt[, mod])
  # add vars for spotlight analysis
  dt[, iv_binary_flipped := fcase(iv_binary == 0, 1, iv_binary == 1, 0)]
  dt[, mod_minus_mod_low := mod - (mod_mean - mod_sd)]
  dt[, mod_minus_mod_mean := mod - mod_mean]
  dt[, mod_minus_mod_high := mod - (mod_mean + mod_sd)]
  # 6 regression models as a list
  reg_models <- list(
    lm(formula = dv ~ iv_binary * mod_minus_mod_low, dt),
    lm(formula = dv ~ iv_binary_flipped * mod_minus_mod_low, dt),
    lm(formula = dv ~ iv_binary * mod_minus_mod_mean, dt),
    lm(formula = dv ~ iv_binary_flipped * mod_minus_mod_mean, dt),
    lm(formula = dv ~ iv_binary * mod_minus_mod_high, dt),
    lm(formula = dv ~ iv_binary_flipped * mod_minus_mod_high, dt))
  # p values of the simple effect
  simple_effect_p_values <- vapply(1:3, function(i) {
    summary(reg_models[[(i - 1) * 2 + 1]])[[
      "coefficients"]]["iv_binary", "Pr(>|t|)"]
  }, FUN.VALUE = numeric(1L))
  # estimated dv and its se and 95% ci
  estimated_dv_dt <- data.table::data.table(do.call(rbind, lapply(
    seq_along(reg_models), function(i) {
    coeff_matrix <- summary(reg_models[[i]])[["coefficients"]]
    estimated_dv <- coeff_matrix["(Intercept)", "Estimate"]
    se_of_intercept <- coeff_matrix["(Intercept)", "Std. Error"]
    dv_minus_1_se <- dv - se_of_intercept
    dv_plus_1_se <- dv + se_of_intercept
    ci_95_ll <- stats::confint(reg_models[[i]])["(Intercept)", 1]
    ci_95_ul <- stats::confint(reg_models[[i]])["(Intercept)", 2]
    output <- c(
      estimated_dv = estimated_dv,
      dv_minus_1_se = dv_minus_1_se,
      dv_plus_1_se = dv_plus_1_se,
      ci_95_ll = ci_95_ll,
      ci_95_ul = ci_95_ul)
    return(output)
  })))
  # dt for plotting
  dt2 <- data.table::data.table(
    iv = factor(rep(1:2, 3), labels = c(
      as.character(iv_level_1), as.character(iv_level_2))),
    mod = factor(
      rep(1:3, each = 2),
      labels = c("-1 SD", "Mean", "+1 SD")),
    estimated_dv_dt)
  # ggplot base
  g1 <- ggplot2::ggplot(data = dt2, ggplot2::aes(
    x = mod, y = estimated_dv, color = iv, group = iv))
  # the error bars will overlap,
  # so use position_dodge to move them horizontally
  pd <- ggplot2::position_dodge(width = position_dodge)
  # points and lines
  if (lines_connecting_means == TRUE) {
    g1 <- g1 + ggplot2::geom_line(size = line_size, position = pd)
  }
  g1 <- g1 + ggplot2::geom_point(size = dot_size, position = pd)
  g1 <- g1 + ggplot2::labs(color = iv_name)
  # add error bars
  if (error_bar == "ci") {
    g1 <- g1 + ggplot2::geom_errorbar(ggplot2::aes(
      ymin = dt2$ci_95_ll, ymax = dt2$ci_95_ul),
      width = error_bar_tip_width,
      size = error_bar_thickness,
      position = pd)
    error_bar_desc_text <- paste0(
      error_bar_range * 100, "% confidence intervals")
  }
  if (error_bar == "se") {
    g1 <- g1 + ggplot2::geom_errorbar(ggplot2::aes(
      ymin = dt2$dv_minus_1_se,
      ymax = dt2$dv_plus_1_se),
      width = error_bar_tip_width,
      size = error_bar_thickness,
      position = pd)
    error_bar_desc_text <- "one standard error (+/- 1 SE)"
  }
  # change colors
  g1 <- g1 + ggplot2::scale_color_manual(values = colors)
  # add brackets for simple effects
  focal_values <- kim::su(dt2[, mod])
  for (i in seq_along(focal_values)) {
    # set the x coordinate
    x_coordinate <- i + position_dodge * 1.2
    # y values at focal values
    y_coordinates <- dt2[mod == focal_values[i], estimated_dv]
    # add simple effect text
    g1 <- g1 + ggplot2::annotate(
      geom = "text",
      x = i + position_dodge * 3,
      y = mean(y_coordinates),
      label = paste0(
        "Simple Effect\n", kim::pretty_round_p_value(
          simple_effect_p_values[i], include_p_equals = TRUE)),
      color = simple_effect_color,
      hjust = 0.5,
      fontface = "bold",
      size = simple_effect_font_size)
    # vertical portion of the segment
    g1 <- g1 + ggplot2::geom_segment(
      x = x_coordinate,
      y = y_coordinates[1],
      xend = x_coordinate,
      yend = y_coordinates[2],
      color = simple_effect_color,
      size = line_size)
    # horizontal portion of the segment
    for (j in 1:2) {
      g1 <- g1 + ggplot2::geom_segment(
        x = x_coordinate,
        y = y_coordinates[j],
        xend = x_coordinate - position_dodge / 2,
        yend = y_coordinates[j],
        color = simple_effect_color,
        size = line_size)
    }
  }
  # include interaction p value
  x_range <- length(kim::su(dt2[, mod])) - 1
  y_range <- max(dt[, dv], na.rm = TRUE) - min(dt[, dv], na.rm = TRUE)
  if (interaction_p_include == TRUE) {
    lm_summary <- summary(stats::lm(dv ~ iv_binary * mod, data = dt))
    interaction_p_value <- kim::pretty_round_p_value(
      lm_summary[["coefficients"]]["iv_binary:mod", "Pr(>|t|)"],
      include_p_equals = TRUE,
      round_digits_after_decimal = round_decimals_int_p_value)
    interaction_p_value_text <- paste0(
      "Interaction ", interaction_p_value)
    # label interaction p value
    g1 <- g1 + ggplot2::annotate(
      geom = "text",
      x = 1 + x_range * 0.5,
      y = Inf,
      label = interaction_p_value_text,
      hjust = 0.5, vjust = -3,
      fontface = "bold",
      color = "black",
      size = interaction_p_value_font_size)
  }
  # plot theme
  g1 <- g1 + kim::theme_kim(
    y_axis_title_vjust = y_axis_title_vjust,
    legend_position = legend_position)
  # allow labeling outside the plot area
  suppressMessages(g1 <- g1 + ggplot2::coord_cartesian(clip = "off"))
  g1 <- g1 + ggplot2::theme(plot.margin = plot_margin)
  # label axes and legend
  # x axis title
  if (is.null(x_axis_title)) {
    g1 <- g1 + ggplot2::xlab(mod_name)
  } else {
    if (x_axis_title == FALSE) {
      g1 <- g1 + ggplot2::theme(axis.title.x = element_blank())
    } else {
      g1 <- g1 + ggplot2::xlab(x_axis_title)
    }
  }
  # y axis title
  if (is.null(y_axis_title)) {
    g1 <- g1 + ggplot2::ylab(dv_name)
  } else {
    if (y_axis_title == FALSE) {
      g1 <- g1 + ggplot2::theme(axis.title.y = element_blank())
    } else {
      g1 <- g1 + ggplot2::ylab(y_axis_title)
    }
  }
  # legend title
  if (is.null(legend_title)) {
    g1 <- g1 + ggplot2::labs(
      color = iv_name,
      linetype = iv_name)
  } else {
    if (legend_title == FALSE) {
      g1 <- g1 + ggplot2::theme(legend.title = element_blank())
    } else {
      g1 <- g1 + ggplot2::labs(color = legend_title)
    }
  }
  # caption about error bars
  g1 <- g1 + ggplot2::labs(caption = paste0(
    "\nError bars indicate ", error_bar_desc_text, " around the mean."))
  # output
  if (output_type == "plot") {
    return(g1)
  }
}
