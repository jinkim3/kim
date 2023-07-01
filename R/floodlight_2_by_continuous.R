#' Floodlight 2 by Continuous
#'
#' Conduct a floodlight analysis for 2 x Continuous design.
#'
#' The following package(s) must be installed prior to running this function:
#' Package 'interactions' v1.1.1 (or possibly a higher version) by
#' Jacob A. Long (2020),
#' <https://cran.r-project.org/package=interactions>
#' See the following references:
#' Spiller et al. (2013) \doi{10.1509/jmr.12.0420}
#' Kim (2021) \doi{10.5281/zenodo.4445388}
#'
#' @param data a data object (a data frame or a data.table)
#' @param iv_name name of the binary independent variable
#' @param dv_name name of the dependent variable
#' @param mod_name name of the continuous moderator variable
#' @param covariate_name name of the variables to control for
#' @param interaction_p_include logical. Should the plot include a
#' p-value for the interaction term?
#' @param iv_level_order order of levels in the independent
#' variable for legend. By default, it will be set as levels of the
#' independent variable ordered using R's base function \code{sort}.
#' @param output type of output (default = "reg_lines_plot").
#' Possible inputs: "interactions_pkg_results", "simple_effects_plot",
#' "jn_points", "regions", "reg_lines_plot"
#' @param jitter_x_percent horizontally jitter dots by a percentage of the
#' range of x values
#' @param jitter_y_percent vertically jitter dots by a percentage of the
#' range of y values
#' @param dot_alpha opacity of the dots (0 = completely transparent,
#' 1 = completely opaque). By default, \code{dot_alpha = 0.5}
#' @param dot_size size of the dots (default = 4)
#' @param interaction_p_value_font_size font size for the interaction
#' p value (default = 8)
#' @param jn_point_font_size font size for Johnson-Neyman point labels
#' (default = 6)
#' @param jn_point_label_hjust a vector of hjust values for
#' Johnson-Neyman point labels. By default, the hjust value will be 0.5 for
#' all the points.
#' @param interaction_p_vjust By how much should the label for the
#' interaction p-value be adjusted vertically?
#' By default, \code{interaction_p_vjust = -3})
#' @param plot_margin margin for the plot
#' By default \code{plot_margin = ggplot2::unit(c(75, 7, 7, 7), "pt")}
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
#' @param round_jn_point_labels To how many digits after the
#' decimal point should the jn point labels be rounded? (default = 2)
#' @param line_of_fit_thickness thickness of the lines of fit (default = 1)
#' @examples
#' \donttest{
#' # typical example
#' floodlight_2_by_continuous(
#' data = mtcars,
#' iv_name = "am",
#' dv_name = "mpg",
#' mod_name = "qsec")
#' # add covariates
#' floodlight_2_by_continuous(
#' data = mtcars,
#' iv_name = "am",
#' dv_name = "mpg",
#' mod_name = "qsec",
#' covariate_name = c("cyl", "hp"))
#' }
#' # adjust the jn point label positions
#' floodlight_2_by_continuous(
#' data = mtcars,
#' iv_name = "am",
#' dv_name = "mpg",
#' mod_name = "qsec",
#' jn_point_label_hjust = c(1, 0))
#' # return regions of significance and nonsignificance
#' floodlight_2_by_continuous(
#' data = mtcars,
#' iv_name = "am",
#' dv_name = "mpg",
#' mod_name = "qsec",
#' output = "regions")
#' @export
#' @import data.table
floodlight_2_by_continuous <- function(
    data = NULL,
    iv_name = NULL,
    dv_name = NULL,
    mod_name = NULL,
    covariate_name = NULL,
    interaction_p_include = TRUE,
    iv_level_order = NULL,
    output = "reg_lines_plot",
    jitter_x_percent = 0,
    jitter_y_percent = 0,
    dot_alpha = 0.5,
    dot_size = 4,
    interaction_p_value_font_size = 8,
    jn_point_font_size = 6,
    jn_point_label_hjust = NULL,
    plot_margin = ggplot2::unit(c(75, 7, 7, 7), "pt"),
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
    round_decimals_int_p_value = 3,
    line_of_fit_thickness = 1,
    round_jn_point_labels = 2
) {
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
  # check if Package 'interactions' is installed
  if (!"interactions" %in% installed_pkgs) {
    message(paste0(
      "To conduct floodlight analysis, Package 'interactions' must ",
      "be installed.\nTo install Package 'interactions', type ",
      "'kim::prep(interactions)'",
      "\n\nAlternatively, to install all packages (dependencies) required ",
      "for all\nfunctions in Package 'kim', type ",
      "'kim::install_all_dependencies()'"))
    return()
  } else {
    # proceed if Package 'interactions' is already installed
    jn_fn_from_interactions <- utils::getFromNamespace(
      "johnson_neyman", "interactions")
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
  dv <- iv <- iv_binary <- iv_factor <- mod <- NULL
  # convert to data.table
  dt <- data.table::setDT(data.table::copy(data))
  # remove columns not needed for analysis
  dt <- dt[, c(iv_name, dv_name, mod_name, covariate_name), with = FALSE]
  # remove rows with na
  dt <- stats::na.omit(dt)
  # order and rename columns
  data.table::setcolorder(dt, c(
    iv_name, dv_name, mod_name, covariate_name))
  # give temporary names to covariates
  if (length(covariate_name) > 0) {
    cov_temp_names <- paste0("cov_", seq_along(covariate_name))
    names(dt) <- c("iv", "dv", "mod", cov_temp_names)
  } else {
    names(dt) <- c("iv", "dv", "mod")
  }
  # unique values in iv
  iv_unique_values <- sort(unique(dt[, iv]))
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
    iv == iv_level_1, 0,
    iv == iv_level_2, 1)]
  # add a factor
  dt[, iv_factor := factor(
    iv_binary,
    levels = 0:1,
    labels = c(as.character(iv_level_1), as.character(iv_level_2)))]
  # lm formula
  if (!is.null(covariate_name)) {
    lm_formula <- stats::as.formula(paste0(
      "dv ~ iv_binary * mod + ",
      paste0(cov_temp_names, collapse = " + ")))
  } else {
    lm_formula <- dv ~ iv_binary * mod
  }
  # find jn points
  johnson_neyman_result <- jn_fn_from_interactions(
    stats::lm(formula = lm_formula, data = dt),
    pred = iv_binary,
    modx = mod)
  # is the significant region inside or outside
  sig_inside_vs_outside <- ifelse(
    attributes(johnson_neyman_result)$inside, "inside", "outside")
  # return the results from the interactions package
  if (output == "interactions_pkg_results") {
    return(johnson_neyman_result)
  }
  # get jn points
  jn_points <- johnson_neyman_result[["bounds"]]
  # return jn points
  if (output == "jn_points") {
    return(jn_points)
  }
  # get regions of significance and nonsignficance
  if (output == "regions") {
    regions_of_sig_nonsig <- data.table::data.table(
      t(jn_points), sig_inside_vs_outside)
    names(regions_of_sig_nonsig) <-
      tolower(names(regions_of_sig_nonsig))
    return(regions_of_sig_nonsig)
  }
  # if there are more than 2 jn points, throw an error
  if (length(jn_points) > 2) {
    message(paste0(
      "An internal computation suggests that there are more than\n",
      "two Johnson-Neyman points. Whether or not this is theoretically\n",
      "possible, the current version of the function cannot proceed\n",
      "with more than two Johnson-Neyman points."))
    return()
  }
  # min and max of observed mod
  mod_min_observed <- min(dt[, mod])
  mod_max_observed <- max(dt[, mod])
  # find the overlapping regions
  if (sig_inside_vs_outside == "inside") {
    sig_region <- list(kim::overlapping_interval(
      mod_min_observed, mod_max_observed,
      jn_points[["Lower"]], jn_points[["Higher"]]))
  } else if (sig_inside_vs_outside == "outside") {
    sig_region <- list(
      kim::overlapping_interval(
        mod_min_observed, mod_max_observed,
        -Inf, jn_points[["Lower"]]),
      kim::overlapping_interval(
        mod_min_observed, mod_max_observed,
        jn_points[["Higher"]], Inf))
  }
  # shade the following regions
  sig_region <- Filter(Negate(is.null), sig_region)
  # plot simple effects at values of moderator
  if (output == "simple_effects_plot") {
    g1 <- johnson_neyman_result[["plot"]]
    g1 <- g1 + ggplot2::ggtitle(paste0(
      "Johnson-Neyman Plot - ",
      "R Package 'interactions', Jacob A. Long (2020)"))
    g1 <- g1 + ggplot2::xlab(mod_name)
    g1 <- g1 + ggplot2::ylab(paste0("Slope of ", iv_name))
    return(g1)
  }
  # jitter
  x_range <- max(dt[, mod], na.rm = TRUE) - min(dt[, mod], na.rm = TRUE)
  y_range <- max(dt[, dv], na.rm = TRUE) - min(dt[, dv], na.rm = TRUE)
  # plot
  g1 <- ggplot2::ggplot(
    data = dt,
    ggplot2::aes(
      x = mod, y = dv,
      color = iv_factor,
      linetype = iv_factor))
  # plot points but make them transparent if covariates are used
  if (!is.null(covariate_name)) {
    dot_alpha <- 0
    dot_alpha <- 0
  }
  g1 <- g1 + ggplot2::geom_point(
    size = dot_size,
    alpha = dot_alpha,
    position = ggplot2::position_jitter(
      width = x_range * jitter_x_percent / 100,
      height = y_range * jitter_y_percent / 100))
  # plot regression lines
  if (is.null(covariate_name)) {
    g1 <- g1 + ggplot2::geom_smooth(
      formula = y ~ x,
      method = "lm",
      se = FALSE,
      linewidth = line_of_fit_thickness)
    g1 <- g1 + ggplot2::scale_linetype_manual(
      values = reg_line_types)
  }
  # include interaction p value
  if (interaction_p_include == TRUE) {
    lm_summary <- summary(stats::lm(formula = lm_formula, data = dt))
    interaction_p_value <- kim::pretty_round_p_value(
      lm_summary[["coefficients"]]["iv_binary:mod", "Pr(>|t|)"],
      include_p_equals = TRUE,
      round_digits_after_decimal = round_decimals_int_p_value)
    interaction_p_value_text <- paste0(
      "Interaction ", interaction_p_value)
    # label interaction p value
    g1 <- g1 + ggplot2::annotate(
      geom = "text",
      x = min(dt[, mod]) + x_range * 0.5,
      y = Inf,
      label = interaction_p_value_text,
      hjust = 0.5, vjust = -3,
      fontface = "bold",
      color = "black",
      size = interaction_p_value_font_size)
  }
  # apply the theme beforehand
  g1 <- g1 + kim::theme_kim(legend_position = legend_position)
  # allow labeling outside the plot area
  suppressMessages(g1 <- g1 + ggplot2::coord_cartesian(clip = "off"))
  g1 <- g1 + ggplot2::theme(
    plot.margin = plot_margin)
  # if only one type is entered for jn line
  if (length(jn_line_types) == 1) {
    jn_line_types <- rep(jn_line_types, length(unlist(sig_region)))
  }
  # add a vertical line and label for each jn point
  if (length(sig_region) > 0) {
    for (i in seq_along(sig_region)) {
      # range of the sig region
      temp_range <- sig_region[[i]]
      # shade the sig region
      g1 <- g1 + ggplot2::annotate(
        "rect", xmin = temp_range[1], xmax = temp_range[2],
        ymin = -Inf, ymax = Inf,
        alpha = sig_region_alpha, fill = sig_region_color)
      for (j in seq_along(temp_range)) {
        # vertical line
        g1 <- g1 + ggplot2::geom_vline(
          xintercept = temp_range[j],
          linetype = jn_line_types[j],
          linewidth = 1)
        # label jn points
        if (is.null(jn_point_label_hjust)) {
          jn_point_label_hjust <- rep(0.5, length(temp_range))
        }
        g1 <- g1 + ggplot2::annotate(
          geom = "text",
          x = temp_range[j],
          y = Inf,
          label = round(temp_range[j], round_jn_point_labels),
          hjust = jn_point_label_hjust[j], vjust = -0.5,
          fontface = "bold",
          color = "black",
          size = jn_point_font_size)
      }
    }
  }
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
      g1 <- g1 + ggplot2::labs(
        color = legend_title,
        linetype = legend_title)
    }
  }
  # add a note on covariates if applicable
  if (!is.null(covariate_name)) {
    g1 <- g1 + ggplot2::labs(caption = paste0(
      "Covariates (Variables Controlled for):\n",
      paste0(covariate_name, collapse = ", ")))
  }
  return(g1)
}
