#' Spotlight 2 by Continuous
#'
#' Conduct a spotlight analysis for a 2 x Continuous design.
#' See Spiller et al. (2013) <doi:10.1509/jmr.12.0420>.
#'
#' @param data a data object (a data frame or a data.table)
#' @param iv_name name of the binary independent variable (IV)
#' @param dv_name name of the dependent variable (DV)
#' @param mod_name name of the continuous moderator variable (MOD)
#' @param covariate_name name(s) of the variable(s) to control for in
#' estimating conditional values of the DV.
#' @param focal_values focal values of the moderator variable at which
#' to estimate IV's effect on DV.
#' @param iv_level_order order of levels in the independent
#' variable for legend. By default, it will be set as levels of the
#' independent variable ordered using R's base function \code{sort}.
#' @param mean_center_vars name(s) of the variable(s) to mean-center.
#' @param output_type type of output (default = "plot").
#' @param colors set colors for the two levels of the independent variable
#' By default, \code{colors = c("red", "blue")}.
#' @param observed_dots logical. If \code{observed_dots = TRUE}, the
#' observed values of all IV, DV, and MOD combinations will be plotted as
#' dots. On top of these dots the spotlight analysis plot will be laid.
#' If \code{observed_dots = FALSE}, these dots will not be plotted.
#' By default, \code{observed_dots = FALSE}.
#' @param dot_size size of the observed_dots (default = 3)
#' @param reg_lines logical. If \code{reg_lines = TRUE}, the regression
#' lines from regressing DV on MOD at each value of IV will be plotted.
#' If \code{reg_lines = FALSE}, these regression lines will not be plotted.
#' By default, \code{observed_dots = FALSE}.
#' @param reg_line_size thickness of the regression lines (default = 1).
#' @param lines_connecting_est_dv logical. Should lines connecting the
#' estimated values of DV be drawn? (default = TRUE)
#' @param lines_connecting_est_dv_size thickness of the lines connecting the
#' estimated values of DV (default = 1).
#' @param estimated_dv_dot_shape ggplot value for shape of the dots
#' at estimated values of DV (default = 15, a square shape).
#' @param estimated_dv_dot_size size of the dots at estimated values of
#' DV (default = 6).
#' @param error_bar if \code{error_bar = "se"}; error bars will be +/-1
#' standard error, if \code{error_bar = "ci"} error bars will be a
#' confidence interval. By default, \code{error_bar = "ci"}.
#' @param error_bar_range width of the confidence interval
#' (default = 0.95 for a 95 percent confidence interval).
#' This argument will not apply when \code{error_bar = "se"}
#' @param error_bar_tip_width graphically, width of the segments
#' at the end of error bars (default = 0.13)
#' @param error_bar_thickness thickness of the error bars (default = 1)
#' @param error_bar_tip_width_percent description not ready
#' @param error_bar_offset description not ready
#' @param error_bar_offset_percent description not ready
#' @param simp_eff_bracket_leg_ht description not ready
#' @param simp_eff_bracket_leg_ht_perc description not ready
#' @param simp_eff_bracket_offset description not ready
#' @param simp_eff_bracket_offset_perc description not ready
#' @param simp_eff_bracket_color description not ready
#' @param simp_eff_bracket_line_size description not ready
#' @param simp_eff_text_offset description not ready
#' @param simp_eff_text_offset_percent description not ready
#' @param simp_eff_text_hjust description not ready
#' @param simp_eff_text_part_1 The first part of the text for
#' labeling simple effects.
#' By default, \code{simp_eff_text_part_1 = "Simple Effect\n"}
#' @param simp_eff_text_color color for the text indicating p-values
#' of simple effects (default = "black").
#' @param simp_eff_font_size font size of the text indicating
#' p-values of simple effects (default = 5).
#' @param interaction_p_include logical. Should the plot include a
#' p-value for the interaction term?
#' @param interaction_p_value_x description not ready
#' @param interaction_p_value_y description not ready
#' @param interaction_p_value_font_size font size for the interaction
#' p value (default = 6)
#' @param interaction_p_value_vjust description not ready
#' @param interaction_p_value_hjust description not ready
#' @param x_axis_breaks description not ready
#' @param x_axis_limits description not ready
#' @param x_axis_tick_mark_labels description not ready
#' @param y_axis_breaks description not ready
#' @param y_axis_limits description not ready
#' @param x_axis_space_left_perc description not ready
#' @param x_axis_space_right_perc description not ready
#' @param y_axis_tick_mark_labels description not ready
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
#' @param jitter_x_percent horizontally jitter dots by a percentage of the
#' range of x values
#' @param jitter_y_percent vertically jitter dots by a percentage of the
#' range of y values
#' @param dot_alpha opacity of the dots (0 = completely transparent,
#' 1 = completely opaque). By default, \code{dot_alpha = 0.2}
#' @param reg_line_alpha description not ready
#' @param jn_point_font_size description not ready
#' @param reg_line_types types of the regression lines for the two levels
#' of the independent variable.
#' By default, \code{reg_line_types = c("solid", "dashed")}
#' @param caption description not ready
#' @param plot_margin margin for the plot
#' By default \code{plot_margin = ggplot2::unit(c(60, 30, 7, 7), "pt")}
#' @param silent If \code{silent = FALSE}, (various) messages will be
#' printed. If \code{silent = TRUE}, the messages will be suppressed.
#' By default, \code{silent = FALSE}.
#' @examples
#' \donttest{
#' spotlight_2_by_continuous(
#' data = mtcars,
#' iv_name = "am",
#' dv_name = "mpg",
#' mod_name = "qsec")
#' # control for variables
#' spotlight_2_by_continuous(
#' data = mtcars,
#' iv_name = "am",
#' dv_name = "mpg",
#' mod_name = "qsec",
#' covariate_name = c("cyl", "hp"))
#' # control for variables and adjust simple effect labels
#' spotlight_2_by_continuous(
#' data = mtcars,
#' iv_name = "am",
#' dv_name = "mpg",
#' mod_name = "qsec",
#' covariate_name = c("cyl", "hp"),
#' reg_lines = TRUE,
#' observed_dots = TRUE,
#' error_bar_offset_percent = 3,
#' error_bar_tip_width_percent = 3,
#' simp_eff_text_offset_percent = 3,
#' simp_eff_bracket_leg_ht_perc = 2,
#' dot_alpha = 0.2,
#' simp_eff_text_part_1 = "")
#' # spotlight at specific values
#' spotlight_2_by_continuous(
#' data = mtcars,
#' iv_name = "am",
#' dv_name = "mpg",
#' mod_name = "qsec",
#' covariate_name = c("cyl", "hp"),
#' focal_values = seq(15, 22, 1),
#' reg_lines = TRUE,
#' observed_dots = TRUE,
#' dot_alpha = 0.2,
#' simp_eff_text_part_1 = "",
#' simp_eff_font_size = 4,
#' error_bar_offset_percent = 3,
#' error_bar_tip_width_percent = 3,
#' simp_eff_text_offset_percent = 3,
#' simp_eff_bracket_leg_ht_perc = 1,
#' x_axis_breaks = seq(15, 22, 1))
#' }
#' @export
spotlight_2_by_continuous <- function(
  data = NULL,
  iv_name = NULL,
  dv_name = NULL,
  mod_name = NULL,
  covariate_name = NULL,
  focal_values = NULL,
  interaction_p_include = TRUE,
  iv_level_order = NULL,
  mean_center_vars = NULL,
  output_type = "plot",
  colors = c("red", "blue"),
  dot_size = 3,
  observed_dots = FALSE,
  reg_lines = FALSE,
  reg_line_size = 1,
  lines_connecting_est_dv = TRUE,
  lines_connecting_est_dv_size = 1,
  estimated_dv_dot_shape = 15,
  estimated_dv_dot_size = 6,
  error_bar = "ci",
  error_bar_range = 0.95,
  error_bar_tip_width = NULL,
  error_bar_tip_width_percent = 8,
  error_bar_thickness = 1,
  error_bar_offset = NULL,
  error_bar_offset_percent = 8,
  simp_eff_bracket_leg_ht = NULL,
  simp_eff_bracket_leg_ht_perc = 2,
  simp_eff_bracket_offset = NULL,
  simp_eff_bracket_offset_perc = 1,
  simp_eff_bracket_color = "black",
  simp_eff_bracket_line_size = 1,
  simp_eff_text_offset = NULL,
  simp_eff_text_offset_percent = 7,
  simp_eff_text_hjust = 0.5,
  simp_eff_text_part_1 = "Simple Effect\n",
  simp_eff_text_color = "black",
  simp_eff_font_size = 5,
  interaction_p_value_x = NULL,
  interaction_p_value_y = NULL,
  interaction_p_value_font_size = 6,
  interaction_p_value_vjust = -1,
  interaction_p_value_hjust = 0.5,
  x_axis_breaks = NULL,
  x_axis_limits = NULL,
  x_axis_tick_mark_labels = NULL,
  y_axis_breaks = NULL,
  y_axis_limits = NULL,
  x_axis_space_left_perc = 10,
  x_axis_space_right_perc = 30,
  y_axis_tick_mark_labels = NULL,
  x_axis_title = NULL,
  y_axis_title = NULL,
  legend_title = NULL,
  legend_position = "right",
  y_axis_title_vjust = 0.85,
  round_decimals_int_p_value = 3,
  jitter_x_percent = 0,
  jitter_y_percent = 0,
  dot_alpha = 0.2,
  reg_line_alpha = 0.5,
  jn_point_font_size = 6,
  reg_line_types = c("solid", "dashed"),
  caption = NULL,
  plot_margin = ggplot2::unit(c(60, 30, 7, 7), "pt"),
  silent = FALSE
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
  iv_binary_flipped <- estimated_dv <- mod_minus_focal_value <- NULL
  focal_value <- focal_value_alt <- NULL
  y1_error_bar_x <- y2_error_bar_x <- NULL
  simp_eff_bracket_x_begin <- simp_eff_bracket_x_end <- NULL
  simp_eff_text_x <- mean_of_y1_and_y2 <- NULL
  y1 <- y2 <- simp_eff_p_value <- NULL
  error_bar_ll <- error_bar_ul <- NULL
  # convert to data.table
  dt <- data.table::setDT(data.table::copy(data))
  # save the beginning n
  n_original <- nrow(dt)
  # keep only the vars needed
  cols_to_remove <- setdiff(
    names(dt), c(iv_name, dv_name, mod_name, covariate_name))
  if (length(cols_to_remove) > 0) {
    dt[, (cols_to_remove) := NULL]
  }
  # remove rows with na
  dt <- stats::na.omit(dt)
  n_after_removing_na <- nrow(dt)
  # mean center variables
  if (length(mean_center_vars) > 0) {
    missing_vars_for_mean_centering <- setdiff(
      mean_center_vars, names(dt))
    if (length(missing_vars_for_mean_centering) > 0) {
      stop(paste0(
        "The following variables for mean-centering do not",
        " exist in the data set:", paste0(
          missing_vars_for_mean_centering, collapse = ", ")))
    }
    for (col in mean_center_vars) {
      data.table::set(
        dt, j = col, value = scale(dt[[col]], scale = FALSE))
      message(paste0(
        "The following variables were mean-centered prior to ",
        "regression analyses:", paste0(
          mean_center_vars, collapse = ", ")))
    }
  }
  # print the number of observations removed
  if (silent == FALSE) {
    if (n_after_removing_na < n_original) {
      kim::pm(n_original - n_after_removing_na,
              " observation(s) were removed due to missing values.\n")
    }
  }
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
  # flip iv
  dt[, iv_binary_flipped := data.table::fcase(
    iv_binary == 0, 1, iv_binary == 1, 0)]
  # lm formulas
  lm_formula_1_character <-
    "dv ~ iv_binary * mod_minus_focal_value"
  lm_formula_2_character <-
    "dv ~ iv_binary_flipped * mod_minus_focal_value"
  lm_formula_main_character <-
    "dv ~ iv_binary * mod"
  if (!is.null(covariate_name)) {
    lm_formula_1_character <- paste0(
      lm_formula_1_character, " + ",
      paste0(cov_temp_names, collapse = " + "))
    lm_formula_2_character <- paste0(
      lm_formula_2_character, " + ",
      paste0(cov_temp_names, collapse = " + "))
    lm_formula_main_character <- paste0(
      lm_formula_main_character, " + ",
      paste0(cov_temp_names, collapse = " + "))
  }
  lm_formula_1 <- stats::as.formula(lm_formula_1_character)
  lm_formula_2 <- stats::as.formula(lm_formula_2_character)
  lm_formula_main <- stats::as.formula(lm_formula_main_character)
  # focal values of the moderator
  if (is.null(focal_values)) {
    mod_mean <- mean(dt[, mod])
    mod_sd <- stats::sd(dt[, mod])
    focal_values <- c(
      mod_mean - mod_sd,
      mod_mean,
      mod_mean + mod_sd
    )
  } else {
    # order the unique values
    focal_values <- kim::su(focal_values)
  }
  # check if a plot superimposition is necessary
  overlay <- ifelse(
    observed_dots == FALSE & reg_lines == FALSE, FALSE, TRUE)
  # set values related to focal values
  if (overlay == TRUE) {
    focal_values_alt <- focal_values
    focal_value_description <- focal_values
  } else if (overlay == FALSE) {
    focal_values_alt <- 1:3
    focal_value_description <- factor(1:3, labels = c(
      "-1 SD", "Mean", "+1 SD"))
  }
  # conduct spotlight regressions
  spotlight_results <- lapply(seq_along(focal_values), function(i) {
    # create a column for mod minus the focal value of the iteration
    dt[, mod_minus_focal_value := mod - focal_values[i]]
    # regression model with iv normal
    lm_1 <- stats::lm(formula = lm_formula_1, dt)
    # simple effect p value
    simp_eff_p_value <- summary(lm_1)[[
      "coefficients"]]["iv_binary", "Pr(>|t|)"]
    # estimated dv when iv = 0
    y1 <- summary(lm_1)[["coefficients"]]["(Intercept)", "Estimate"]
    # error bar
    if (error_bar == "ci") {
      error_bar_range_y1 <- stats::confint(lm_1, level = error_bar_range)[
        "(Intercept)", ]
      names(error_bar_range_y1) <- c(
        "y1_error_bar_begin", "y1_error_bar_end")
    } else if (error_bar == "se") {
      se_of_intercept <- summary(lm_1)[["coefficients"]][
        "(Intercept)", "Std. Error"]
      error_bar_range_y1 <- c(y1 - se_of_intercept, y1 + se_of_intercept)
      names(error_bar_range_y1) <- c(
        "y1_error_bar_begin", "y1_error_bar_end")
    }
    # regression model with iv flipped
    lm_2 <- stats::lm(formula = lm_formula_2, dt)
    # estimated dv when iv = 0
    y2 <- summary(lm_2)[["coefficients"]]["(Intercept)", "Estimate"]
    # error bar
    if (error_bar == "ci") {
      error_bar_range_y2 <- stats::confint(lm_2, level = error_bar_range)[
        "(Intercept)", ]
      names(error_bar_range_y2) <- c(
        "y2_error_bar_begin", "y2_error_bar_end")
    } else if (error_bar == "se") {
      se_of_intercept <- summary(lm_2)[["coefficients"]][
        "(Intercept)", "Std. Error"]
      error_bar_range_y2 <- c(y1 - se_of_intercept, y1 + se_of_intercept)
      names(error_bar_range_y2) <- c(
        "y2_error_bar_begin", "y2_error_bar_end")
    }
    # output
    output <- c(
      y1 = y1, error_bar_range_y1, y2 = y2, error_bar_range_y2,
      simp_eff_p_value = simp_eff_p_value)
    return(output)
  })
  # spotlight regression results as a data table
  dt2 <- data.table::data.table(
    do.call(rbind, spotlight_results))
  # add focal values
  dt2[, focal_value := focal_values]
  dt2[, focal_value_alt := focal_values_alt]
  dt2[, focal_value_description := focal_value_description]
  # parameters for plotting
  # min and max of x and y
  if (overlay == TRUE) {
    x_min <- min(dt[, mod], na.rm = TRUE)
    x_max <- max(dt[, mod], na.rm = TRUE)
    y_min <- min(dt[, dv], na.rm = TRUE)
    y_max <- max(dt[, dv], na.rm = TRUE)
    y_range <- y_max - y_min
  } else if (overlay == FALSE) {
    x_min <- min(dt2[, focal_value_alt])
    x_max <- max(dt2[, focal_value_alt])
  }
  # x and y ranges
  x_range <- x_max - x_min
  # offset error bars so they do not overlap
  if (is.null(error_bar_offset)) {
    error_bar_offset <- x_range * error_bar_offset_percent / 100
  }
  error_bar_offset_half <- error_bar_offset / 2
  # error bar tip width
  if (is.null(error_bar_tip_width)) {
    error_bar_tip_width <- x_range * error_bar_tip_width_percent / 100
  }
  # add x coordinates of error bars
  dt2[, y1_error_bar_x := focal_value_alt - error_bar_offset_half]
  dt2[, y2_error_bar_x := focal_value_alt + error_bar_offset_half]
  # add x coordinates of simple effect brackets
  if (is.null(simp_eff_bracket_offset)) {
    simp_eff_bracket_offset <-
      x_range * simp_eff_bracket_offset_perc / 100
  }
  if (is.null(simp_eff_bracket_leg_ht)) {
    simp_eff_bracket_leg_ht <-
      x_range * simp_eff_bracket_leg_ht_perc / 100
  }
  dt2[, simp_eff_bracket_x_begin := focal_value_alt +
        error_bar_offset_half + simp_eff_bracket_offset]
  dt2[, simp_eff_bracket_x_end := focal_value_alt +
        error_bar_offset_half + simp_eff_bracket_offset +
        simp_eff_bracket_leg_ht]
  # add x coordinates of simple effect text
  if (is.null(simp_eff_text_offset)) {
    simp_eff_text_offset <-
      x_range * simp_eff_text_offset_percent / 100
  }
  dt2[, simp_eff_text_x :=
        simp_eff_bracket_x_end + simp_eff_text_offset]
  # add y coordinates of simple effect text
  dt2[, mean_of_y1_and_y2 :=
        rowMeans(dt2[, c("y1", "y2"), with = FALSE])]
  # error bar type
  if (error_bar == "ci") {
    error_bar_desc_text <- paste0(
      error_bar_range * 100, "% confidence intervals")
  } else if (error_bar == "se") {
    error_bar_desc_text <- "one standard error (+/- 1 SE)"
  }
  # begin plotting if there is nothing to overlay
  if (overlay == FALSE) {
    # parameters for plotting
    pd <- ggplot2::position_dodge(width = error_bar_offset)
    # edit dt for plotting
    dt3 <- dt2[, c(
      "y1", "y1_error_bar_begin", "y1_error_bar_end",
      "focal_value_description")]
    dt3[, iv := iv_level_1]
    dt4 <- dt2[, c(
      "y2", "y2_error_bar_begin", "y2_error_bar_end",
      "focal_value_description")]
    dt4[, iv := iv_level_2]
    dt5 <- rbind(dt3, dt4, use.names = FALSE)
    names(dt5) <- c(
      "estimated_dv", "error_bar_ll", "error_bar_ul", "mod", "iv")
    dt5[, iv := factor(iv, levels = c(iv_level_1, iv_level_2))]
    # begin plotting
    g1 <- ggplot2::ggplot(data = dt5, mapping = ggplot2::aes(
      x = mod, y = estimated_dv, color = iv, group = iv))
    g1 <- g1 + ggplot2::scale_color_manual(values = colors)
    # add error bars
    g1 <- g1 + ggplot2::geom_errorbar(ggplot2::aes(
      ymin = error_bar_ll, ymax = error_bar_ul),
      width = error_bar_tip_width,
      size = error_bar_thickness,
      position = pd)
    # lines connecting the estimated dvs at focal values
    if (lines_connecting_est_dv == TRUE) {
      g1 <- g1 + ggplot2::geom_line(
        size = lines_connecting_est_dv_size,
        position = pd)
    }
    # estimated dvs at focal values
    g1 <- g1 + ggplot2::geom_point(
      size = estimated_dv_dot_size,
      shape = estimated_dv_dot_shape,
      position = pd)
    # remove the lines through legend
    g1 <- g1 + ggplot2::guides(
      fill = ggplot2::guide_legend(override.aes = list(linetype = 0)),
      color = ggplot2::guide_legend(override.aes = list(linetype = 0)))
  } else if (overlay == TRUE) {
    # begin plotting
    # ggplot base
    g1 <- ggplot2::ggplot(data = dt, ggplot2::aes(
      x = mod, y = dv, color = iv_factor, group = iv_factor,
      linetype = iv_factor))
    g1 <- g1 + ggplot2::scale_color_manual(values = colors)
    # add dots
    if (observed_dots == TRUE) {
      g1 <- g1 + ggplot2::geom_point(
        size = dot_size,
        alpha = dot_alpha,
        position = ggplot2::position_jitter(
          width = x_range * jitter_x_percent / 100,
          height = y_range * jitter_y_percent / 100))
    }
    # add regression lines
    if (reg_lines == TRUE) {
      g1 <- g1 + ggplot2::geom_line(
        stat = "smooth",
        formula = y ~ x,
        method = "lm", se = FALSE,
        size = reg_line_size,
        alpha = reg_line_alpha)
      g1 <- g1 + ggplot2::scale_linetype_manual(
        values = reg_line_types)
    }
    # add estimated values of dv from spotlight regressions
    g1 <- g1 + ggplot2::geom_point(
      data = dt2, mapping = ggplot2::aes(
        x = focal_value - error_bar_offset_half, y = y1),
      color = colors[1],
      size = estimated_dv_dot_size,
      shape = estimated_dv_dot_shape)
    g1 <- g1 + ggplot2::geom_point(
      data = dt2, mapping = ggplot2::aes(
        x = focal_value + error_bar_offset_half, y = y2),
      color = colors[2],
      size = estimated_dv_dot_size,
      shape = estimated_dv_dot_shape)
    # add error bars from spotlight regressions
    if (error_bar %in% c("se", "ci")) {
      g1 <- g1 + ggplot2::geom_errorbar(data = dt2, ggplot2::aes(
        x = y1_error_bar_x, y = y1,
        ymin = dt2$y1_error_bar_begin, ymax = dt2$y1_error_bar_end),
        inherit.aes = FALSE,
        width = error_bar_tip_width,
        size = error_bar_thickness,
        color = colors[1])
      g1 <- g1 + ggplot2::geom_errorbar(data = dt2, ggplot2::aes(
        x = y2_error_bar_x, y = y2,
        ymin = dt2$y2_error_bar_begin, ymax = dt2$y2_error_bar_end),
        inherit.aes = FALSE,
        width = error_bar_tip_width,
        size = error_bar_thickness,
        color = colors[2])
      if (error_bar == "ci") {
        error_bar_desc_text <- paste0(
          error_bar_range * 100, "% confidence intervals")
      } else if (error_bar == "se") {
        error_bar_desc_text <- "one standard error (+/- 1 SE)"
      }
    }
    # connect estimates from spotlight regressions
    if (lines_connecting_est_dv == TRUE) {
      g1 <- g1 + ggplot2::geom_line(
        data = dt2, ggplot2::aes(
          x = y1_error_bar_x, y = y1),
        inherit.aes = FALSE,
        size = lines_connecting_est_dv_size,
        color = colors[1])
      g1 <- g1 + ggplot2::geom_line(
        data = dt2, ggplot2::aes(
          x = y2_error_bar_x, y = y2),
        inherit.aes = FALSE,
        size = lines_connecting_est_dv_size,
        color = colors[2])
    }
  }
  # add simple effect brackets and texts
  for (i in seq_along(focal_values_alt)) {
    # x coordinates of the bracket
    temp_bracket_x_begin <- dt2[
      focal_value_alt == focal_values_alt[i], simp_eff_bracket_x_begin]
    temp_bracket_x_end <- dt2[
      focal_value_alt == focal_values_alt[i], simp_eff_bracket_x_end]
    # y coordinates of the bracket ends
    temp_bracket_y_begin <- dt2[focal_value_alt == focal_values_alt[i], y1]
    temp_bracket_y_end <- dt2[focal_value_alt == focal_values_alt[i], y2]
    # the vertical segment of the bracket
    g1 <- g1 + ggplot2::geom_segment(
      x = temp_bracket_x_end,
      y = temp_bracket_y_begin,
      xend = temp_bracket_x_end,
      yend = temp_bracket_y_end,
      color = simp_eff_bracket_color,
      size = simp_eff_bracket_line_size,
      inherit.aes = FALSE)
    # the horizontal segment at the bottom of the bracket
    g1 <- g1 + ggplot2::geom_segment(
      x = temp_bracket_x_begin,
      y = temp_bracket_y_begin,
      xend = temp_bracket_x_end,
      yend = temp_bracket_y_begin,
      color = simp_eff_bracket_color,
      size = simp_eff_bracket_line_size,
      inherit.aes = FALSE)
    # the horizontal segment at the top of the bracket
    g1 <- g1 + ggplot2::geom_segment(
      x = temp_bracket_x_begin,
      y = temp_bracket_y_end,
      xend = temp_bracket_x_end,
      yend = temp_bracket_y_end,
      color = simp_eff_bracket_color,
      size = simp_eff_bracket_line_size,
      inherit.aes = FALSE)
    # x coordinate of the simple effect texts
    temp_simp_eff_text_x <- dt2[
      focal_value_alt == focal_values_alt[i], simp_eff_text_x]
    # y coordinate of the simple effect texts
    temp_simp_eff_text_y <- dt2[
      focal_value_alt == focal_values_alt[i], mean_of_y1_and_y2]
    # add simple effect text
    g1 <- g1 + ggplot2::annotate(
      geom = "text",
      x = temp_simp_eff_text_x,
      y = temp_simp_eff_text_y,
      label = paste0(
        simp_eff_text_part_1, kim::pretty_round_p_value(
          dt2[focal_value_alt == focal_values_alt[i], simp_eff_p_value],
          include_p_equals = TRUE)),
      color = simp_eff_text_color,
      hjust = simp_eff_text_hjust,
      fontface = "bold",
      size = simp_eff_font_size)
  }
  # include interaction p value
  if (interaction_p_include == TRUE) {
    lm_summary <- summary(stats::lm(
      formula = lm_formula_main, data = dt))
    interaction_p_value <- kim::pretty_round_p_value(
      lm_summary[["coefficients"]]["iv_binary:mod", "Pr(>|t|)"],
      include_p_equals = TRUE,
      round_digits_after_decimal = round_decimals_int_p_value)
    interaction_p_value_text <- paste0(
      "Interaction ", interaction_p_value)
    # label interaction p value
    # x coordinate of the interaction p value label
    if (is.null(interaction_p_value_x)) {
      interaction_p_value_x <- x_min + x_range * 0.5
    }
    # y coordinate of the interaction p value label
    if (is.null(interaction_p_value_y)) {
      interaction_p_value_y <- Inf
    }
    g1 <- g1 + ggplot2::annotate(
      geom = "text",
      x = interaction_p_value_x,
      y = interaction_p_value_y,
      label = interaction_p_value_text,
      hjust = interaction_p_value_hjust,
      vjust = interaction_p_value_vjust,
      fontface = "bold",
      color = "black",
      size = interaction_p_value_font_size)
  }
  # edit x axis
  if (!is.null(x_axis_limits) & !is.null(x_axis_breaks)) {
    if (!is.null(x_axis_tick_mark_labels)) {
      g1 <- g1 + ggplot2::scale_x_continuous(
        limits = x_axis_limits,
        breaks = x_axis_breaks,
        labels = x_axis_tick_mark_labels)
    } else {
      g1 <- g1 + ggplot2::scale_x_continuous(
        limits = x_axis_limits,
        breaks = x_axis_breaks)
    }
  } else if (!is.null(x_axis_limits)) {
    g1 <- g1 + ggplot2::scale_x_continuous(limits = x_axis_limits)
  } else if (!is.null(x_axis_breaks)) {
    g1 <- g1 + ggplot2::scale_x_continuous(breaks = x_axis_breaks)
  }
  # edit y axis
  if (!is.null(y_axis_limits) & !is.null(y_axis_breaks)) {
    if (!is.null(y_axis_tick_mark_labels)) {
      g1 <- g1 + ggplot2::scale_y_continuous(
        limits = y_axis_limits,
        breaks = y_axis_breaks,
        labels = y_axis_tick_mark_labels)
    } else {
      g1 <- g1 + ggplot2::scale_y_continuous(
        limits = y_axis_limits,
        breaks = y_axis_breaks)
    }
  } else if (!is.null(y_axis_limits)) {
    g1 <- g1 + ggplot2::scale_y_continuous(limits = y_axis_limits)
  } else if (!is.null(y_axis_breaks)) {
    g1 <- g1 + ggplot2::scale_y_continuous(breaks = y_axis_breaks)
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
  if (is.null(caption)) {
    g1 <- g1 + ggplot2::labs(caption = paste0(
      "\nError bars indicate ", error_bar_desc_text,
      " around the estimated values."))
  } else if (caption != FALSE) {
    g1 <- g1 + ggplot2::labs(caption = caption)
  }
  # output
  if (output_type == "plot") {
    return(g1)
  }
}
