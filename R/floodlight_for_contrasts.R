#' Floodlight Analyses for a Set of Contrasts
#'
#' Conduct a floodlight analysis for a set of contrasts with a
#' continuous moderator variable.
#'
#' See the following reference, which covers a related topic:
#' Hayes & Montoya (2017) \doi{10.1080/19312458.2016.1271116}
#'
#' @param data a data object (a data frame or a data.table)
#' @param iv_name name of the multicategorical independent variable;
#' this variable must have three or more categories.
#' @param dv_name name of the dependent variable
#' @param mod_name name of the continuous moderator variable
#' @param contrasts names of the contrast variables
#' @param contrasts_for_floodlight names of the contrast variables for
#' which floodlight analyses will be conducted
#' @param covariate_name name of the variables to control for
#' @param interaction_p_include logical. Should the plot include a
#' p-value for the interaction term?
#' @param iv_category_order order of levels in the independent
#' variable for legend. By default, it will be set as levels of the
#' independent variable ordered using R's base function \code{sort}.
#' @param heteroskedasticity_consistent_se which kind of
#' heteroskedasticity-consistent (robust) standard errors should be
#' calculated? (default = "HC4")
#' @param round_r_squared number of decimal places to which to round
#' r-squared values (default = 3)
#' @param round_f number of decimal places to which to round
#' the f statistic for model comparison (default = 2)
#' @param sigfigs number of significant digits to round to
#' (for values in the regression tables, except for p values).
#' By default \code{sigfigs = 2}
#' @param jn_points_disregard_threshold the Minimum Distance in
#' the unit of the moderator variable that will be used for various purposes,
#' such as (1) to disregard the second Johnson-Neyman point
#' that is different from the first Johnson-Neyman (JN) point by
#' less than the Minimum Distance; (2) to determine regions of
#' significance, which will calculate the p-value of the IV's effect
#' (the focal dummy variable's effect) on DV at a candidate
#' JN point + / - the Minimum Distance.
#' This input is hard to explain, but a user can enter a really low value
#' for this argument (e.g., \code{jn_points_disregard_threshold = 0.1}
#' for a moderator measured on a 100-point scale) or use the default.
#' By default, \code{
#' jn_points_disregard_threshold = range of the moderator / 10000}
#' For example, if the observed moderator values range from 1 to 7
#' (because it is a 7-point scale), then \code{
#' jn_points_disregard_threshold = (7 - 1) / 10000 = 0.0006}
#' @param print_floodlight_plots If \code{print_floodlight_plots = TRUE},
#' a floodlight plot for each dummy variable will be printed.
#' By default, \code{print_floodlight_plots = TRUE}
#' @param output output of the function (default = "all").
#' Possible inputs: "reg_models", "reg_tables", "reg_tables_rounded",
#' "all"
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
#' @param line_of_fit_types types of the lines of fit for the two levels
#' of the independent variable.
#' By default, \code{line_of_fit_types = c("solid", "dashed")}
#' @param line_of_fit_thickness thickness of the lines of fit (default = 1.5)
#' @param jn_line_types types of the lines for Johnson-Neyman points.
#' By default, \code{jn_line_types = c("solid", "solid")}
#' @param jn_line_thickness thickness of the lines at Johnson-Neyman points
#' (default = 1.5)
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
#' @examples
#' \dontrun{
#' # typical example
#' # copy and modify the 'mtcars' data
#' mtcars2 <- setDT(data.table::copy(mtcars))
#' # make sure the data table package is attached
#' mtcars2[, contrast_1 := fcase(cyl == 4, -2, cyl %in% c(6, 8), 1)]
#' mtcars2[, contrast_2 := fcase(cyl == 4, 0, cyl == 6, 1, cyl == 8, -1)]
#' floodlight_for_contrasts(
#' data = mtcars2,
#' iv_name = "cyl",
#' dv_name = "mpg",
#' mod_name = "qsec",
#' contrasts = paste0("contrast_", 1:2),
#' contrasts_for_floodlight = "contrast_2")
#' }
#' @export
#' @import data.table
floodlight_for_contrasts <- function(
    data = NULL,
    iv_name = NULL,
    dv_name = NULL,
    mod_name = NULL,
    contrasts = NULL,
    contrasts_for_floodlight = NULL,
    covariate_name = NULL,
    interaction_p_include = TRUE,
    iv_category_order = NULL,
    heteroskedasticity_consistent_se = "HC4",
    round_r_squared = 3,
    round_f = 2,
    sigfigs = 2,
    jn_points_disregard_threshold = NULL,
    print_floodlight_plots = TRUE,
    output = "reg_lines_plot",
    jitter_x_percent = 0,
    jitter_y_percent = 0,
    dot_alpha = 0.5,
    dot_size = 4,
    interaction_p_value_font_size = 6,
    jn_point_font_size = 6,
    jn_point_label_hjust = NULL,
    interaction_p_vjust = -3,
    plot_margin = ggplot2::unit(c(75, 7, 7, 7), "pt"),
    legend_position = "right",
    line_of_fit_types = c("solid", "dashed"),
    line_of_fit_thickness = 1.5,
    jn_line_types = c("solid", "solid"),
    jn_line_thickness = 1.5,
    sig_region_color = "green",
    sig_region_alpha = 0.08,
    nonsig_region_color = "gray",
    nonsig_region_alpha = 0.08,
    x_axis_title = NULL,
    y_axis_title = NULL,
    legend_title = NULL,
    round_decimals_int_p_value = 3,
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
  # check if Package 'lmtest' is installed
  if (!"lmtest" %in% installed_pkgs) {
    message(paste0(
      "This function requires the installation of Package 'lmtest'.",
      "\nTo install Package 'lmtest', type ",
      "'kim::prep(lmtest)'",
      "\n\nAlternatively, to install all packages (dependencies) required ",
      "for all\nfunctions in Package 'kim', type ",
      "'kim::install_all_dependencies()'"))
    return()
  } else {
    # proceed if Package 'lmtest' is already installed
    coeftest_fn_from_lmtest <- utils::getFromNamespace(
      "coeftest", "lmtest")
  }
  # check if Package 'sandwich' is installed
  if (!"sandwich" %in% installed_pkgs) {
    message(paste0(
      "This function requires the installation of Package 'sandwich'.",
      "\nTo install Package 'sandwich', type ",
      "'kim::prep(sandwich)'",
      "\n\nAlternatively, to install all packages (dependencies) required ",
      "for all\nfunctions in Package 'kim', type ",
      "'kim::install_all_dependencies()'"))
    return()
  } else {
    # proceed if Package 'sandwich' is already installed
    vcovHC_fn_from_sandwich <- utils::getFromNamespace(
      "vcovHC", "sandwich")
  }
  # check if Package 'DEoptim' is installed
  if (!"DEoptim" %in% installed_pkgs) {
    message(paste0(
      "This function requires the installation of Package 'DEoptim'.",
      "\nTo install Package 'DEoptim', type ",
      "'kim::prep(DEoptim)'",
      "\n\nAlternatively, to install all packages (dependencies) required ",
      "for all\nfunctions in Package 'kim', type ",
      "'kim::install_all_dependencies()'"))
    return()
  } else {
    # proceed if Package 'DEoptim' is already installed
    DEoptim_fn_from_DEoptim <- utils::getFromNamespace(
      "DEoptim", "DEoptim")
    DEoptim_control_fn_from_DEoptim <- utils::getFromNamespace(
      "DEoptim.control", "DEoptim")
  }
  # check whether all arguments had required inputs
  if (is.null(iv_name)) {
    stop("Please enter a variable name for the input 'iv_name'")
  }
  if (is.null(contrasts)) {
    stop("Please enter variable names for the input 'contrasts'")
  }
  if (is.null(dv_name)) {
    stop("Please enter a variable name for the input 'dv_name'")
  }
  if (is.null(mod_name)) {
    stop("Please enter a variable name for the input 'mod_name'")
  }
  # bind the vars locally to the function
  dv <- iv <- iv_binary <- iv_factor <- mod <- p <-
    mod_temp <- segment_x <- segment_xend <- segment_y <- segment_yend <-
    variable <- NULL
  # number of all contrasts
  num_of_contrasts <- length(contrasts)
  # number of contrasts for floodlight
  num_of_contrasts_for_floodlight <- length(contrasts_for_floodlight)
  # convert to data.table
  dt <- data.table::setDT(data.table::copy(data))
  # remove columns not needed for analysis
  dt <- dt[, c(
    iv_name, contrasts, dv_name, mod_name, covariate_name), with = FALSE]
  # remove rows with na
  dt <- stats::na.omit(dt)
  # order and rename columns
  data.table::setcolorder(dt, c(
    iv_name, contrasts, dv_name, mod_name, covariate_name))
  # give temporary names to covariates
  if (length(covariate_name) > 0) {
    cov_temp_names <- paste0("cov_", seq_along(covariate_name))
    names(dt) <- c("iv", contrasts, "dv", "mod", cov_temp_names)
  } else {
    names(dt) <- c("iv", contrasts, "dv", "mod")
  }
  # print the coding scheme
  cat(paste0("Contrast Coding Scheme:\n"))
  contrast_coding_table <- unique(dt[, c(
    "iv", contrasts), with = FALSE])
  data.table::setnames(contrast_coding_table, "iv", iv_name)
  data.table::setorderv(contrast_coding_table, cols = iv_name)
  print(contrast_coding_table)
  cat("\n")
  # formula for the initial model
  lm_1_formula_character <- paste0(
    "dv ~ ", paste0(
      contrasts, collapse = " + "),
    " + mod + ",
    paste0(paste0(
      contrasts, ":mod"), collapse = " + "))
  # estimate the model without floodlight
  lm_1_formula <- stats::as.formula(lm_1_formula_character)
  # estimate the model
  lm_1 <- stats::lm(formula = lm_1_formula, data = dt)
  if (output == "reg_model") {
    return(lm_1)
  }
  # use heteroskedasticity consistent standard errors
  if (heteroskedasticity_consistent_se == FALSE) {
    message(
      "Heteroskedacity-Consistent Standard Errors were NOT calculated.")
    lm_1_coeff_only <- summary(lm_1)$coefficients
  } else {
    message(paste0(
      "Heteroskedacity-Consistent Standard Errors are calculated using",
      " the ", heteroskedasticity_consistent_se, " estimator."))
    lm_1_w_hc_se <- coeftest_fn_from_lmtest(
      lm_1, vcov. = vcovHC_fn_from_sandwich(
        lm_1, type = heteroskedasticity_consistent_se))
    lm_1_coeff_only <- lm_1_w_hc_se[,]
  }
  # create regression tables
  lm_1_reg_table <- data.table::data.table(
    variable = row.names(lm_1_coeff_only),
    lm_1_coeff_only)
  names(lm_1_reg_table) <- c("variable", "b", "se_b", "t", "p")
  if (output == "reg_table") {
    return(lm_1_reg_table)
  }
  # round values in the reg tables
  lm_1_reg_table_rounded <- data.table::copy(lm_1_reg_table)
  for (x in c("b", "se_b", "t")) {
    data.table::set(
      lm_1_reg_table_rounded, j = x,
      value = kim::round_flexibly(
        lm_1_reg_table_rounded[[x]], sigfigs = sigfigs))
  }
  # column names in the reg tables
  names(lm_1_reg_table_rounded) <-
    c("Variable", "B", "SE B", "t", "p")
  # round the p values
  lm_1_reg_table_rounded[, p := kim::pretty_round_p_value(p)]
  if (output == "reg_table_rounded") {
    return(lm_1_reg_table_rounded)
  }
  # print the initial model
  cat(paste0("\nModel 1: ", lm_1_formula_character, "\n"))
  print(lm_1_reg_table_rounded)
  # copy the dt
  dt2 <- data.table::copy(dt)
  # formula to use for floodlight
  floodlight_lm_formula <- do.call(
    "substitute", list(lm_1_formula, list(mod = quote(mod_temp))))
  # min and max of observed mod
  mod_min_observed <- min(dt[, mod])
  mod_max_observed <- max(dt[, mod])
  # x range is the same as mod range, because mod is plotted along the x axis
  mod_range <- x_range <- mod_max_observed - mod_min_observed
  # function for optimizing to find the mod values at which p value = 0.05
  function_to_find_jn_points <- function(
    x = NULL,
    data = NULL,
    lm_formula = NULL,
    predictor_in_regression = NULL,
    target_p_value = 0.05) {
    data[, mod_temp := mod - x]
    temp_lm_summary <- summary(
      stats::lm(formula = lm_formula, data = data))
    temp_p <- temp_lm_summary$coefficients[
      predictor_in_regression, "Pr(>|t|)"]
    output <- abs(temp_p - target_p_value)
    return(output)
  }
  # find jn points for each dummy variable
  floodlight_plots <- jn_points_by_contrast <-
    vector(mode = "list", length = num_of_contrasts)
  # create floodlight plots
  for (i in seq_along(contrasts_for_floodlight)) {
    # clear the results from the previous iteration
    temp_optim_results_1 <- temp_optim_results_2 <-
      temp_optim_results_3 <- temp_jn_points <- jn_points_verified <-
      jn_point_p_distance_from_ref_p <- temp_lm_summary <-
      num_of_jn_points <- jn_point_dt_final <-
      most_likely_jn_point_1 <- temp_mod_value_1 <-
      temp_mod_value_1 <- temp_mod_value_2 <-
      temp_mod_value_3 <- temp_mod_value_4 <-
      sig_region <- temp_p_1 <- temp_p_2 <- temp_p_3 <- NULL
    cat(paste0(
      "\nSearching for JN points for ",
      contrasts_for_floodlight[i], " ..."))
    # disregard the second jn point based on threshold
    if (is.null(jn_points_disregard_threshold)) {
      jn_points_disregard_threshold <- mod_range / 10 ^ 4
    }
    # use deoptim to find the jn points
    # optimizing 1 of 3
    temp_optim_results_1 <- DEoptim_fn_from_DEoptim(
      fn = function_to_find_jn_points,
      lower = mod_min_observed,
      upper = mod_max_observed,
      control = DEoptim_control_fn_from_DEoptim(trace = FALSE),
      data = dt2,
      lm_formula = floodlight_lm_formula,
      predictor_in_regression = contrasts_for_floodlight[i])
    jn_point_candidate_1 <- temp_optim_results_1$optim$bestmem
    # optimizing 2 of 3
    temp_optim_results_2 <- DEoptim_fn_from_DEoptim(
      fn = function_to_find_jn_points,
      lower = mod_min_observed,
      upper = jn_point_candidate_1,
      control = DEoptim_control_fn_from_DEoptim(trace = FALSE),
      data = dt2,
      lm_formula = floodlight_lm_formula,
      predictor_in_regression = contrasts_for_floodlight[i])
    jn_point_candidate_2 <- temp_optim_results_2$optim$bestmem
    # optimizing 3 of 3
    temp_optim_results_3 <- DEoptim_fn_from_DEoptim(
      fn = function_to_find_jn_points,
      lower = jn_point_candidate_1,
      upper = mod_max_observed,
      control = DEoptim_control_fn_from_DEoptim(trace = FALSE),
      data = dt2,
      lm_formula = floodlight_lm_formula,
      predictor_in_regression = contrasts_for_floodlight[i])
    jn_point_candidate_3 <- temp_optim_results_3$optim$bestmem
    # gather the jn points
    temp_jn_points <- unname(c(
      jn_point_candidate_1,
      jn_point_candidate_2,
      jn_point_candidate_3))
    jn_points_verified <- c()
    jn_point_p_distance_from_ref_p <- c()
    # verify the jn points
    for (j in seq_along(temp_jn_points)) {
      temp_jn_point_to_verify <- temp_jn_points[j]
      dt2[, mod_temp := mod - temp_jn_point_to_verify]
      temp_lm_summary <- summary(
        stats::lm(formula = floodlight_lm_formula, data = dt2))
      temp_p <- temp_lm_summary$coefficients[
        contrasts_for_floodlight[i], "Pr(>|t|)"]
      # p value can be off by 0.0001
      if (abs(temp_p - 0.05) < 0.0001) {
        jn_points_verified <- c(jn_points_verified, temp_jn_points[j])
        jn_point_p_distance_from_ref_p <- c(
          jn_point_p_distance_from_ref_p, abs(temp_p - 0.05))
      }
    }
    # continue only if jn points were verified
    if (length(jn_points_verified) == 0) {
      num_of_jn_points <- 0
    } else if (length(jn_points_verified) > 0) {
      if (length(jn_points_verified) == 1) {
        jn_point_dt_final <- data.table::data.table(
          jn_points_verified, jn_point_p_distance_from_ref_p)
      } else if (length(jn_points_verified) %in% 2:3) {
        temp_jn_point_dt <- data.table::data.table(
          jn_points_verified, jn_point_p_distance_from_ref_p)
        data.table::setorder(
          temp_jn_point_dt, jn_point_p_distance_from_ref_p)
        most_likely_jn_point_1 <-
          temp_jn_point_dt[1, jn_points_verified]
        if (length(jn_points_verified) == 2) {
          if (abs(most_likely_jn_point_1 -
                  temp_jn_point_dt[2, jn_points_verified]) >
              jn_points_disregard_threshold) {
            jn_point_dt_final <- temp_jn_point_dt[1:2, ]
          } else {
            jn_point_dt_final <- temp_jn_point_dt[1, ]
          }
        } else if (length(jn_points_verified) == 3) {
          most_likely_jn_point_1 <-
            temp_jn_point_dt[1, jn_points_verified]
          if (abs(most_likely_jn_point_1 -
                  temp_jn_point_dt[2, jn_points_verified]) >
              jn_points_disregard_threshold) {
            jn_point_dt_final <- temp_jn_point_dt[1:2, ]
          } else {
            if (abs(most_likely_jn_point_1 -
                    temp_jn_point_dt[3, jn_points_verified]) >
                jn_points_disregard_threshold) {
              jn_point_dt_final <- temp_jn_point_dt[c(1, 3), ]
            } else {
              jn_point_dt_final <- temp_jn_point_dt[1, ]
            }
          }
        }
      }
      # sort the jn points table
      data.table::setorder(jn_point_dt_final, jn_points_verified)
      jn_points_final <- jn_point_dt_final[, jn_points_verified]
      if (!is.null(jn_points_verified)) {
        jn_points_by_contrast[[i]] <- jn_points_final
      }
      names(jn_points_by_contrast)[i] <- contrasts_for_floodlight[i]
      num_of_jn_points <- length(jn_points_final)
      # print the jn points table
      cat(paste0("\nJN Points for ", contrasts_for_floodlight[i], ":\n"))
      print(jn_point_dt_final)
      # regions of significance
      # if there are more than 2 jn points, throw an error
      if (num_of_jn_points > 2) {
        stop(paste0(
          "An internal computation suggests that there are more than\n",
          "two Johnson-Neyman points. Whether or not this is theoretically\n",
          "possible, the current version of the function cannot proceed\n",
          "with more than two Johnson-Neyman points."))
      }
      if (num_of_jn_points == 0) {
        # if there are no jn points, then either the entire range
        # of the moderator values is sig or nonsig
        # find out which is the case
        # p value at mod min
        temp_mod_value_1 <- mod_min_observed
        dt2[, mod_temp := mod - temp_mod_value_1]
        temp_p_1 <- summary(stats::lm(
          formula = floodlight_lm_formula, data = dt2))$coefficients[
            contrasts_for_floodlight[i], "Pr(>|t|)"]
        # p value at mod max
        temp_mod_value_2 <- mod_max_observed
        dt2[, mod_temp := mod - temp_mod_value_2]
        temp_p_2 <- summary(stats::lm(
          formula = floodlight_lm_formula, data = dt2))$coefficients[
            contrasts_for_floodlight[i], "Pr(>|t|)"]
        # determine regions of sig
        if (temp_p_1 >= 0.05 & temp_p_2 >= 0.05) {
          sig_region <- NULL
        } else if (temp_p_1 < 0.05 & temp_p_2 < 0.05) {
          sig_region <- list(c(mod_min_observed, mod_max_observed))
        } else {
          stop(paste0(
            "An error occurred while determining regions of significance",
            "\n(Error Code 101)."))
        }
      } else if (num_of_jn_points == 1) {
        # if there is exactly 1 jn point, then either side of the
        # jn point is sig; find out which side
        # p value at mod min
        temp_mod_value_1 <-
          jn_points_final - jn_points_disregard_threshold
        dt2[, mod_temp := mod - temp_mod_value_1]
        temp_p_1 <- summary(stats::lm(
          formula = floodlight_lm_formula, data = dt2))$coefficients[
            contrasts_for_floodlight[i], "Pr(>|t|)"]
        # p value at mod max
        temp_mod_value_2 <-
          jn_points_final + jn_points_disregard_threshold
        dt2[, mod_temp := mod - temp_mod_value_2]
        temp_p_2 <- summary(stats::lm(
          formula = floodlight_lm_formula, data = dt2))$coefficients[
            contrasts_for_floodlight[i], "Pr(>|t|)"]
        # determine regions of sig
        if (temp_p_1 < 0.05 & temp_p_2 >= 0.05) {
          sig_region <- list(c(mod_min_observed, jn_points_final))
        } else if (temp_p_1 >= 0.05 & temp_p_2 < 0.05) {
          sig_region <- list(c(jn_points_final, mod_max_observed))
        } else {
          stop(paste0(
            "An error occurred while determining regions of significance",
            "\n(Error Code 102)."))
        }
      } else if (num_of_jn_points == 2) {
        # if there are 2 jn points, then either the middle region is sig
        # or the two regions on both sides are sig
        # p value at mod min
        temp_mod_value_1 <- mod_min_observed + jn_points_disregard_threshold
        dt2[, mod_temp := mod - temp_mod_value_1]
        temp_p_1 <- summary(stats::lm(
          formula = floodlight_lm_formula, data = dt2))$coefficients[
            contrasts_for_floodlight[i], "Pr(>|t|)"]
        # p value in the middle region
        temp_mod_value_2 <- mean(c(
          max(jn_points_final), min(jn_points_final)))
        dt2[, mod_temp := mod - temp_mod_value_2]
        temp_p_2 <- summary(stats::lm(
          formula = floodlight_lm_formula, data = dt2))$coefficients[
            contrasts_for_floodlight[i], "Pr(>|t|)"]
        # p value at mod max
        temp_mod_value_3 <- mod_max_observed - jn_points_disregard_threshold
        dt2[, mod_temp := mod - temp_mod_value_3]
        temp_p_3 <- summary(stats::lm(
          formula = floodlight_lm_formula, data = dt2))$coefficients[
            contrasts_for_floodlight[i], "Pr(>|t|)"]
        # determine regions of sig
        if (temp_p_1 < 0.05 & temp_p_2 >= 0.05 & temp_p_3 < 0.05) {
          sig_region <- list(
            c(mod_min_observed, jn_points_final[1]),
            c(jn_points_final[2], mod_max_observed))
        } else if (temp_p_1 >= 0.05 & temp_p_2 < 0.05 & temp_p_3 >= 0.05) {
          sig_region <- list(
            c(jn_points_final[1], jn_points_final[2]))
        } else {
          stop(paste0(
            "An error occurred while determining regions of significance",
            "\n(Error Code 103)."))
        }
      }
    }
    # dt for plotting
    # print(paste0("flood", i))
    temp_dt <- dt[
      get(contrasts_for_floodlight[i]) != 0]
    temp_dt[, iv_factor := factor(iv)]
    # min and max of observed dv
    dv_min_observed <- min(temp_dt[, dv])
    dv_max_observed <- max(temp_dt[, dv])
    # range of y (the dv)
    y_range <- dv_max_observed - dv_min_observed
    # begin plottting
    g1 <- ggplot2::ggplot(
      data = temp_dt,
      ggplot2::aes(
        x = mod,
        y = dv,
        color = iv_factor,
        linetype = iv_factor))
    # shade the regions of sig
    for (j in seq_along(sig_region)) {
      # range of the sig region
      temp_range <- sig_region[[j]]
      # shade the sig region
      g1 <- g1 + ggplot2::annotate(
        "rect", xmin = temp_range[1], xmax = temp_range[2],
        ymin = dv_min_observed, ymax = dv_max_observed,
        alpha = sig_region_alpha, fill = sig_region_color)
    }
    # plot points but make them transparent if covariates are used
    if (!is.null(covariate_name)) {
      dot_alpha <- 0
      dot_alpha <- 0
    }
    # jitter
    g1 <- g1 + ggplot2::geom_point(
      size = dot_size,
      alpha = dot_alpha,
      position = ggplot2::position_jitter(
        width = x_range * jitter_x_percent / 100,
        height = y_range * jitter_y_percent / 100))
    g1 <- g1 + kim::theme_kim(
      cap_axis_lines = TRUE,
      legend_position = legend_position)
    g1 <- g1 + ggplot2::theme(
      legend.spacing.y = ggplot2::unit(1, "cm"),
      legend.key.size = ggplot2::unit(3, "lines"))
    g1 <- g1 + ggplot2::scale_color_manual(values = c("red", "blue"))
    # add the lines of fit
    temp_data_for_predicting <- contrast_coding_table[
      get(iv_name) %in% unique(temp_dt[, iv_factor])]
    temp_data_for_predicting <- temp_data_for_predicting[
      rep(seq_len(nrow(temp_data_for_predicting)),
          each = 2), ]
    temp_data_for_predicting[, mod := rep(
      c(mod_min_observed, mod_max_observed), 2)]
    # predicted dv values
    predicted_dv <- stats::predict(lm_1, temp_data_for_predicting)
    dt_for_lines_of_fit <- contrast_coding_table[
      get(iv_name) %in% unique(temp_dt[, iv_factor])]
    dt_for_lines_of_fit[, segment_x := mod_min_observed]
    dt_for_lines_of_fit[, segment_xend := mod_max_observed]
    segment_y_coord_dt <- data.table::data.table(
      matrix(predicted_dv, ncol = 2, byrow = TRUE))
    names(segment_y_coord_dt) <- c("segment_y", "segment_yend")
    dt_for_lines_of_fit <- data.table::data.table(
      dt_for_lines_of_fit, segment_y_coord_dt)
    dt_for_lines_of_fit[, iv_factor := factor(get(iv_name))]
    # plot the lines of fit (i.e., regression lines)
    if (is.null(covariate_name)) {
      g1 <- g1 + ggplot2::geom_segment(
        mapping = ggplot2::aes(
          x = segment_x,
          y = segment_y,
          xend = segment_xend,
          yend = segment_yend,
          color = iv_factor,
          linetype = iv_factor),
        linewidth = line_of_fit_thickness,
        data = dt_for_lines_of_fit)
      g1 <- g1 + ggplot2::scale_linetype_manual(
        values = line_of_fit_types)
    }
    # include interaction p value
    if (interaction_p_include == TRUE) {
      interaction_p_value <- kim::pretty_round_p_value(
        lm_1_reg_table[variable == paste0(
          contrasts_for_floodlight[i], ":mod"), p],
        include_p_equals = TRUE,
        round_digits_after_decimal = round_decimals_int_p_value)
      interaction_p_value_text <- paste0(
        "Interaction ", interaction_p_value)
      # label interaction p value
      g1 <- g1 + ggplot2::annotate(
        geom = "text",
        x = mod_min_observed + x_range * 0.5,
        y = Inf,
        label = interaction_p_value_text,
        hjust = 0.5,
        vjust = interaction_p_vjust,
        fontface = "bold",
        color = "black",
        size = interaction_p_value_font_size)
    }
    # allow labeling outside the plot area
    suppressMessages(g1 <- g1 + ggplot2::coord_cartesian(clip = "off"))
    g1 <- g1 + ggplot2::theme(
      plot.margin = plot_margin)
    if (num_of_jn_points %in% 1:2) {
      # jn line types
      # if only one type is entered for jn line
      if (length(jn_line_types) == 1) {
        jn_line_types <- rep(jn_line_types, num_of_jn_points)
      }
      # add the vertical line at jn points
      for (j in seq_along(jn_points_final)) {
        g1 <- g1 + ggplot2::annotate(
          geom = "segment",
          x = jn_points_final[j],
          y = dv_min_observed,
          xend = jn_points_final[j],
          yend = dv_max_observed,
          color = "black",
          linewidth = jn_line_thickness)
        # label jn points
        if (is.null(jn_point_label_hjust)) {
          jn_point_label_hjust <- rep(0.5, num_of_jn_points)
        }
        g1 <- g1 + ggplot2::annotate(
          geom = "text",
          x = jn_points_final[j],
          y = Inf,
          label = round(jn_points_final[j], round_jn_point_labels),
          hjust = jn_point_label_hjust[j], vjust = -0.5,
          fontface = "bold",
          color = "black",
          size = jn_point_font_size)
      }
    }
    # x axis title
    if (is.null(x_axis_title)) {
      g1 <- g1 + ggplot2::xlab(mod_name)
    } else {
      if (x_axis_title == FALSE) {
        g1 <- g1 + ggplot2::theme(axis.title.x = ggplot2::element_blank())
      } else {
        g1 <- g1 + ggplot2::xlab(x_axis_title)
      }
    }
    # y axis title
    if (is.null(y_axis_title)) {
      g1 <- g1 + ggplot2::ylab(dv_name)
    } else {
      if (y_axis_title == FALSE) {
        g1 <- g1 + ggplot2::theme(axis.title.y = ggplot2::element_blank())
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
        g1 <- g1 + ggplot2::theme(legend.title = ggplot2::element_blank())
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
    if (print_floodlight_plots == TRUE) {
      print(g1)
    }
    floodlight_plots[[i]] <- g1
  }
  # output of the function
  fn_output <- list(
    reg_model = lm_1,
    reg_table = lm_1_reg_table,
    reg_table_rounded = lm_1_reg_table_rounded,
    floodlight_plots = floodlight_plots)
  invisible(fn_output)
}
