#' Simplify data for analysis
#'
#' Simplify data for analyses commonly conducted by
#'
#' @param data a data object (a data frame or a data.table)
#' @param iv_name name of the independent variable (IV)
#' @param dv_name name of the dependent variable (DV)
#' @param mod_name name of the moderator variable (MOD)
#' @param na.omit logical. If \code{na.omit = TRUE}, rows containing
#' an NA value in any of the focal variables (e.g., IV, DV, and MOD)
#' will be removed in the simplified data. By default, \code{
#' na.omit = TRUE}
#' @examples
#' \donttest{
#' data_simplify(
#' data = mtcars, iv_name = "vs", dv_name = "mpg", mod_name = "hp")
#' }
#' @export
#' @import data.table
data_simplify <- function(
    data = NULL,
    iv_name = NULL,
    dv_name = NULL,
    mod_name = NULL,
    na.omit = TRUE,
    output_type = "dt"
) {
  # check inputs
  non_null_vars <- Filter(Negate(is.null), c(iv_name, dv_name, mod_name))
  if (is.null(non_null_vars)) {
    stop(paste0(
      "Please enter at least one variable name ",
      "as an input to the function."))
  }
  # copy data
  dt <- setDT(copy(data))[, non_null_vars, with = FALSE]


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
  if (is.null(dv_name)) {
    stop("Please enter a variable name for the input 'dv_name'")
  }
  if (is.null(mod_name)) {
    stop("Please enter a variable name for the input 'mod_name'")
  }
  # bind the vars locally to the function
  dv <- dv_binary <- dv_factor <- iv <- iv_binary <- iv_factor <-
    mod <- b <- se_b <- z <- p <- variable <- mod_temp <- NULL
  # convert to data.table
  dt <- data.table::setDT(data.table::copy(data))
  # remove columns not needed for analysis
  dt <- dt[, c(iv_name, dv_name, mod_name), with = FALSE]
  # remove rows with na
  dt <- stats::na.omit(dt)
  # order and rename columns
  data.table::setcolorder(dt, c(
    iv_name, dv_name, mod_name))
  names(dt) <- c("iv", "dv", "mod")
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
    if (!identical(
      iv_unique_values_character,
      sort(iv_level_order_character))) {
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
  # repeat the above for dv
  # unique values in dv
  dv_unique_values <- sort(unique(dt[, dv]))
  dv_unique_values_character <- as.character(dv_unique_values)
  # check if dv is binary
  num_of_levels_in_dv <- length(dv_unique_values)
  if (num_of_levels_in_dv != 2) {
    stop(paste0(
      "The independent variable has ", num_of_levels_in_dv,
      " levels.\n",
      "The current version of the function can only handle",
      " an independent variable with exactly two levels."))
  }
  # set the order of levels in dv
  if (is.null(dv_level_order)) {
    dv_level_1 <- dv_unique_values[1]
    dv_level_2 <- dv_unique_values[2]
  } else {
    # check if the dv levels match
    dv_level_order_character <- as.character(dv_level_order)
    if (!identical(
      dv_unique_values_character,
      sort(dv_level_order_character))) {
      stop(paste0(
        "\nThe levels of independent variables do not match:\n",
        "dv_level_order input: ",
        paste0(dv_level_order_character, collapse = ", "),
        "\nLevels of DV in the data set: ",
        paste0(dv_unique_values_character, collapse = ", ")))
    }
    dv_level_1 <- dv_level_order[1]
    dv_level_2 <- dv_level_order[2]
  }
  # add binary variable
  dt[, dv_binary := data.table::fcase(
    dv == dv_level_1, 0,
    dv == dv_level_2, 1)]
  # add a factor
  dt[, dv_factor := factor(
    dv_binary,
    levels = 0:1,
    labels = c(as.character(dv_level_1), as.character(dv_level_2)))]
  # run the logistic regression with the interaction term
  logistic_reg_model_formula <- dv_binary ~ iv_binary * mod
  logistic_reg_model <-
    stats::glm(
      logistic_reg_model_formula, data = dt, family = stats::binomial())
  # print the logistic regression table
  logistic_reg_model_table <- summary(
    logistic_reg_model)$coefficients
  logistic_reg_model_table_row_names <-
    row.names(logistic_reg_model_table)
  # change the row names
  logistic_reg_model_table_row_names <- gsub(
    "^iv_binary$", iv_name, logistic_reg_model_table_row_names)
  logistic_reg_model_table_row_names <- gsub(
    "^mod$", mod_name, logistic_reg_model_table_row_names)
  # logistic_reg_model_table_row_names[which(grepl(
  #   "^cov_\\d+$", logistic_reg_model_table_row_names))] <-
  #   covariate_name
  logistic_reg_model_table_row_names <- gsub(
    "^iv_binary:mod$", paste0(
      iv_name, " x ", mod_name),
    logistic_reg_model_table_row_names)
  # convert the regression table to a data.table
  reg_table_1 <- data.table(
    variable = logistic_reg_model_table_row_names,
    logistic_reg_model_table)
  data.table::setnames(
    reg_table_1,
    old = c("Estimate", "Std. Error", "z value", "Pr(>|z|)"),
    new = c("b", "se_b", "z", "p"))
  # round estimates
  reg_table_1[, b := kim::round_flexibly(b, 2)]
  reg_table_1[, se_b := kim::round_flexibly(se_b, 2)]
  reg_table_1[, z := kim::round_flexibly(z, 2)]
  reg_table_1[, p := kim::pretty_round_p_value(p)]
  # print the regression table
  print(reg_table_1)
  # p value for the interaction
  interaction_p <- reg_table_1[
    variable == paste0(iv_name, " x ", mod_name), p]
  # begin floodlight
  # copy the dt
  dt2 <- data.table::copy(dt)
  # formula to use
  floodlight_formula <- do.call(
    "substitute", list(
      logistic_reg_model_formula, list(mod = quote(mod_temp))))
  # min and max of observed mod
  mod_min_observed <- min(dt[, mod])
  mod_max_observed <- max(dt[, mod])
  # x range is the same as mod range, because mod is plotted along the x axis
  mod_range <- x_range <- mod_max_observed - mod_min_observed
  # function for optimizing to find the mod values at which p value = 0.05
  predictor_in_regression <- "iv_binary"
  function_to_find_jn_points <- function(
    x = NULL,
    data = NULL,
    glm_formula = NULL,
    target_p_value = 0.05) {
    data[, mod_temp := mod - x]
    temp_glm_summary_1 <- summary(stats::glm(
      formula = floodlight_formula, data = data,
      family = stats::binomial()))
    temp_p <- temp_glm_summary_1$coefficients[
      predictor_in_regression, "Pr(>|z|)"]
    output <- abs(temp_p - target_p_value)
    return(output)
  }
  # create floodlight plots
  # clear the results from the previous iteration
  temp_optim_results_1 <- temp_optim_results_2 <-
    temp_optim_results_3 <- temp_jn_points <- jn_points_verified <-
    jn_point_p_distance_from_ref_p <- temp_lm_summary <-
    num_of_jn_points <- jn_point_dt_final <-
    most_likely_jn_point_1 <- temp_mod_value_1 <-
    temp_mod_value_1 <- temp_mod_value_2 <-
    temp_mod_value_3 <- temp_mod_value_4 <-
    sig_region <- temp_p_1 <- temp_p_2 <- temp_p_3 <- NULL
  # disregard the second jn point based on threshold
  if (is.null(jn_points_disregard_threshold)) {
    jn_points_disregard_threshold <- mod_range / 10 ^ 4
  }
  # use deoptim to find the jn points
  cat("\nSearching for JN points...\n")
  # optimizing 1 of 3
  temp_optim_results_1 <- DEoptim_fn_from_DEoptim(
    fn = function_to_find_jn_points,
    lower = mod_min_observed,
    upper = mod_max_observed,
    control = DEoptim_control_fn_from_DEoptim(trace = FALSE),
    data = dt2,
    glm_formula = floodlight_formula)
  jn_point_candidate_1 <- temp_optim_results_1$optim$bestmem
  # optimizing 2 of 3
  temp_optim_results_2 <- DEoptim_fn_from_DEoptim(
    fn = function_to_find_jn_points,
    lower = mod_min_observed,
    upper = jn_point_candidate_1,
    control = DEoptim_control_fn_from_DEoptim(trace = FALSE),
    data = dt2,
    glm_formula = floodlight_formula)
  jn_point_candidate_2 <- temp_optim_results_2$optim$bestmem
  # optimizing 3 of 3
  temp_optim_results_3 <- DEoptim_fn_from_DEoptim(
    fn = function_to_find_jn_points,
    lower = jn_point_candidate_1,
    upper = mod_max_observed,
    control = DEoptim_control_fn_from_DEoptim(trace = FALSE),
    data = dt2,
    glm_formula = floodlight_formula)
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
    temp_glm_summary_2 <- summary(stats::glm(
      formula = floodlight_formula, data = dt2,
      family = stats::binomial()))
    temp_p <- temp_glm_summary_2$coefficients[
      predictor_in_regression, "Pr(>|z|)"]
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
  }
  if (length(jn_points_verified) == 1) {
    jn_point_dt_final <- data.table::data.table(
      jn_points_verified, jn_point_p_distance_from_ref_p)
  }
  if (length(jn_points_verified) %in% 2:3) {
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
  if (length(jn_points_verified) > 0) {
    # sort the jn points table
    data.table::setorder(jn_point_dt_final, jn_points_verified)
    jn_points_final <- jn_point_dt_final[, jn_points_verified]
    num_of_jn_points <- length(jn_points_final)
    # print the jn points table
    cat("\nJN Points:\n")
    print(jn_point_dt_final)
  }
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
    temp_p_1 <- summary(stats::glm(
      formula = floodlight_formula, data = dt2,
      family = stats::binomial()))$coefficients[
        predictor_in_regression, "Pr(>|z|)"]
    # p value at mod max
    temp_mod_value_2 <- mod_max_observed
    dt2[, mod_temp := mod - temp_mod_value_2]
    temp_p_2 <- summary(stats::glm(
      formula = floodlight_formula, data = dt2,
      family = stats::binomial()))$coefficients[
        predictor_in_regression, "Pr(>|z|)"]
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
    # p value for the value slightly on the left side
    temp_mod_value_1 <-
      jn_points_final - jn_points_disregard_threshold
    dt2[, mod_temp := mod - temp_mod_value_1]
    temp_p_1 <- summary(stats::glm(
      formula = floodlight_formula, data = dt2,
      family = stats::binomial()))$coefficients[
        predictor_in_regression, "Pr(>|z|)"]
    # p value for the value slightly on the right side
    temp_mod_value_2 <-
      jn_points_final + jn_points_disregard_threshold
    dt2[, mod_temp := mod - temp_mod_value_2]
    temp_p_2 <- summary(stats::glm(
      formula = floodlight_formula, data = dt2,
      family = stats::binomial()))$coefficients[
        predictor_in_regression, "Pr(>|z|)"]
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
    temp_p_1 <- summary(stats::glm(
      formula = floodlight_formula, data = dt2,
      family = stats::binomial()))$coefficients[
        predictor_in_regression, "Pr(>|z|)"]
    # p value in the middle region
    temp_mod_value_2 <- mean(c(
      max(jn_points_final), min(jn_points_final)))
    dt2[, mod_temp := mod - temp_mod_value_2]
    temp_p_2 <- summary(stats::glm(
      formula = floodlight_formula, data = dt2,
      family = stats::binomial()))$coefficients[
        predictor_in_regression, "Pr(>|z|)"]
    # p value at mod max
    temp_mod_value_3 <- mod_max_observed - jn_points_disregard_threshold
    dt2[, mod_temp := mod - temp_mod_value_3]
    temp_p_3 <- summary(stats::glm(
      formula = floodlight_formula, data = dt2,
      family = stats::binomial()))$coefficients[
        predictor_in_regression, "Pr(>|z|)"]
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
  # min and max of observed dv
  dv_min_observed <- min(dt[, dv])
  dv_max_observed <- max(dt[, dv])
  # range of y (the dv)
  y_range <- dv_max_observed - dv_min_observed
  # calculate predicted values
  cat(paste0(
    "Calculating predicted values at different values of the ",
    "moderator...\n"))
  # spotlight values
  spotlight_interval <- mod_range / (num_of_spotlights - 1)
  spotlight_penultimate_value <-
    mod_min_observed + (num_of_spotlights - 2) * spotlight_interval
  spotlight_values <- c(seq(
    mod_min_observed, spotlight_penultimate_value,
    spotlight_interval), mod_max_observed)
  dv_value_when_iv_is_0 <- dv_value_when_iv_is_1 <-
    rep(NA, num_of_spotlights)
  # for now, set the dt for plotting predicted values to NULL
  spotlight_predicted_values_dt <- NULL
  # # notify that the function cannot plot when covariates are included
  # if (length(covariate_name) > 0) {
  #   message(paste0(
  #     "The current version of the function cannot plot predicted",
  #     " values when\ncovariates are included in the model."))
  # }
  # calculate predicted values
  for (i in seq_along(spotlight_values)) {
    dt2[, mod_temp := mod - spotlight_values[i]]
    temp_logistic_reg_model <-
      stats::glm(
        formula = dv_binary ~ iv_binary * mod_temp,
        data = dt2, family = stats::binomial())
    b0 <- temp_logistic_reg_model$coefficients[["(Intercept)"]]
    b1 <- temp_logistic_reg_model$coefficients[["iv_binary"]]
    b2 <- temp_logistic_reg_model$coefficients[["mod_temp"]]
    b3 <- temp_logistic_reg_model$coefficients[["iv_binary:mod_temp"]]
    temp_dv_value_at_iv_0 <- 1 / (1 + exp(-(
      b0 + b1 * 0 + b2 * 0 + b3 * 0)))
    temp_dv_value_at_iv_1 <- 1 / (1 + exp(-(
      b0 + b1 * 1 + b2 * 0 + b3 * 0)))
    dv_value_when_iv_is_0[i] <- temp_dv_value_at_iv_0
    dv_value_when_iv_is_1[i] <- temp_dv_value_at_iv_1
  }
  spotlight_predicted_values_dt <- data.table::data.table(
    mod = rep(spotlight_values, 2),
    iv_factor = factor(
      rep(0:1, each = num_of_spotlights),
      levels = 0:1,
      labels = c(as.character(iv_level_1), as.character(iv_level_2))),
    dv = c(dv_value_when_iv_is_0, dv_value_when_iv_is_1))
  # plot
  g1 <- ggplot2::ggplot(
    data = dt,
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
  # # plot points but make them transparent if covariates are used
  # if (!is.null(covariate_name)) {
  #   dot_alpha <- 0
  # }
  # jitter
  g1 <- g1 + ggplot2::geom_point(
    size = dot_size,
    alpha = dot_alpha,
    position = ggplot2::position_jitter(
      width = x_range * jitter_x_percent / 100,
      height = y_range * jitter_y_percent / 100))
  # add the predicted values
  if (!is.null(spotlight_predicted_values_dt)) {
    g1 <- g1 + ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = mod, y = dv, color = iv_factor, linetype = iv_factor),
      linewidth = line_thickness_for_pred_values,
      data = spotlight_predicted_values_dt)
    g1 <- g1 + ggplot2::scale_linetype_manual(
      values = line_types_for_pred_values)
  }
  g1 <- g1 + kim::theme_kim(
    cap_axis_lines = TRUE,
    legend_position = legend_position)
  g1 <- g1 + ggplot2::theme(
    legend.spacing.y = ggplot2::unit(1, "cm"),
    legend.key.size = ggplot2::unit(3, "lines"))
  g1 <- g1 + ggplot2::scale_color_manual(values = c("red", "blue"))
  # include interaction p value
  if (interaction_p_include == TRUE) {
    interaction_p_value_text <- paste0(
      "Interaction ", interaction_p)
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
      if (jn_point_label_add == TRUE) {
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
  # # add a note on covariates if applicable
  # if (!is.null(covariate_name)) {
  #   g1 <- g1 + ggplot2::labs(caption = paste0(
  #     "Covariates (Variables Controlled for):\n",
  #     paste0(covariate_name, collapse = ", ")))
  # }
  return(g1)
}
