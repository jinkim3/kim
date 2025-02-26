#' Simple slopes analysis with logistic regression analyses
#'
#' Conduct a simple slopes analysis with logistic regression analyses,
#' typically to probe a two-way interaction when the dependent variable
#' is binary.
#'
#' @param data a data object (a data frame or a data.table)
#' @param iv_name name of the independent variable (IV)
#' @param dv_name name of the dependent variable (DV)
#' @param mod_name name of the moderator variable (MOD)
#' @param focal_values this input will be used only in cases where MOD
#' is continuous. In such cases, what are the focal values of the MOD
#' at which to estimate the effect of IV on DV? By default, values
#' corresponding to the mean of MOD, and mean of MOD +/-1 SD will be used.
#' @param round_b number of decimal places to which to round
#' coefficients from the regression analysis (default = 2)
#' @param round_se number of decimal places to which to round
#' standard error values from the regression analysis (default = 2)
#' @param round_z number of decimal places to which to round
#' t statistics from the regression analysis (default = 2)
#' @param round_p number of decimal places to which to round
#' p values from the regression analysis (default = 2)
#' @examples
#' \donttest{
#' simple_slopes_analysis_logistic(
#' data = mtcars, iv_name = "vs", dv_name = "am", mod_name = "hp")
#' simple_slopes_analysis_logistic(
#' data = mtcars, iv_name = "disp", dv_name = "am", mod_name = "hp")
#' }
#' @export
#' @import data.table
simple_slopes_analysis_logistic <- function(
    data = NULL,
    iv_name = NULL,
    dv_name = NULL,
    mod_name = NULL,
    round_b = 2,
    round_se = 2,
    round_z = 2,
    round_p = 3,
    focal_values = NULL
) {
  warning(paste0(
    "This function has not been tested. By using this function, \n",
    "you acknolwedge that it can yield completely incorrect results."))
  # bind the vars locally to the function
  iv <- dv <- mod <- mod_binary_1 <- mod_binary_2 <-
    mod_minus_focal_value <- b <- se <-
    z <- p <- iv_minus_focal_value <- NULL
  # take a subset of data
  dt <- data.table::setDT(data.table::copy(data))
  dt <- dt[, c(iv_name, mod_name, dv_name), with = FALSE]
  # deal with na values
  dt <- stats::na.omit(dt)
  # check if dv is binary
  if (length(unique(dt[[dv_name]])) != 2) {
    stop(paste0(
      "It seems that the DV is not a binary variable.\n",
      "To conduct a simple slopes analysis with ",
      "with a continuous DV,\nuse the function '",
      "simple_slopes_analysis' instead."))
  }
  # formula for the interaction analysis
  glm_formula_for_interaction <- stats::as.formula(paste0(
    dv_name, " ~ ", iv_name, " * ", mod_name))
  # interaction analysis
  kim::logistic_reg_w_interaction(
    data = dt,
    dv_name = dv_name,
    iv_1_name = iv_name,
    iv_2_name = mod_name)
  glm_interaction <- stats::glm(
    formula = glm_formula_for_interaction,
    data = dt, family = stats::binomial())
  glm_interaction_summary <- summary(glm_interaction)
  # name of the interaction term in the model
  reg_table_row_names <- row.names(
    glm_interaction_summary[["coefficients"]])
  interaction_term_name <-
    reg_table_row_names[length(reg_table_row_names)]
  # a colon should be in the name
  if (grepl(":", interaction_term_name) == FALSE) {
    warning(paste0(
      "The function had an issue while locating the\n",
      "interaction term in a regression table.\n",
      "The output of the function may be incorrect."))
  }
  # interaction p
  interaction_b <- glm_interaction_summary[[
    "coefficients"]][interaction_term_name, "Estimate"]
  interaction_se <- glm_interaction_summary[[
    "coefficients"]][interaction_term_name, "Std. Error"]
  interaction_z <- glm_interaction_summary[[
    "coefficients"]][interaction_term_name, "z value"]
  interaction_p <- glm_interaction_summary[[
    "coefficients"]][interaction_term_name, "Pr(>|z|)"]
  results_msg_interaction_sig_text <- data.table::fcase(
    interaction_p < 0.05, "significantly predicted",
    interaction_p >= 0.05, "did not significantly predict",
    default = "[error: interaction_p was not properly calculated]")
  # suppressing the message below since the model comparison and
  # chi-square test seems more appropriate than simply looking at the
  # p value of the interaction term
  # prepare the message output for the interaction analysis
  # interaction_results_msg <- kim::p0(
  #   "\nA multiple regression analysis revealed that\n",
  #   "the interaction between '", iv_name, "' and '", mod_name, "'\n",
  #   results_msg_interaction_sig_text, " '", dv_name, "',\nb = ",
  #   round(interaction_b, round_b), ", SE = ",
  #   round(interaction_se, round_se), ", z = ",
  #   round(interaction_z, round_z), ", ",
  #   kim::pretty_round_p_value(
  #     interaction_p, include_p_equals = TRUE))
  # message(interaction_results_msg)
  # change var names
  names(dt) <- c("iv", "mod", "dv")
  # cases when mod is binary
  if (kim::lenu(dt$mod) == 2) {
    mod_lvl_1 <- sort(unique(dt$mod))[1]
    mod_lvl_2 <- sort(unique(dt$mod))[2]
    dt[, mod_binary_1 := data.table::fcase(
      mod == mod_lvl_1, 0,
      mod == mod_lvl_2, 1)]
    dt[, mod_binary_2 := data.table::fcase(
      mod == mod_lvl_2, 0,
      mod == mod_lvl_1, 1)]
    # simple slope 1
    glm_1 <- stats::glm(
      formula = dv ~ iv * mod_binary_1,
      data = dt, family = stats::binomial())
    glm_1_summary <- summary(glm_1)
    intercept_for_simple_slope_1 <- glm_1_summary[[
      "coefficients"]]["(Intercept)", "Estimate"]
    simple_slope_1_b <- glm_1_summary[["coefficients"]]["iv", "Estimate"]
    simple_slope_1_se <- glm_1_summary[["coefficients"]]["iv", "Std. Error"]
    simple_slope_1_z <- glm_1_summary[["coefficients"]]["iv", "z value"]
    simple_slope_1_p <- glm_1_summary[["coefficients"]]["iv", "Pr(>|z|)"]
    # simple slope 2
    glm_2 <- stats::glm(
      formula = dv ~ iv * mod_binary_2,
      data = dt, family = stats::binomial())
    glm_2_summary <- summary(glm_2)
    intercept_for_simple_slope_2 <- glm_2_summary[[
      "coefficients"]]["(Intercept)", "Estimate"]
    simple_slope_2_b <- glm_2_summary[["coefficients"]]["iv", "Estimate"]
    simple_slope_2_se <- glm_2_summary[["coefficients"]]["iv", "Std. Error"]
    simple_slope_2_z <- glm_2_summary[["coefficients"]]["iv", "z value"]
    simple_slope_2_p <- glm_2_summary[["coefficients"]]["iv", "Pr(>|z|)"]
    # output
    simple_slopes_table <- data.table::data.table(
      focal_value_of_mod = c(mod_lvl_1, mod_lvl_2),
      b = c(simple_slope_1_b, simple_slope_2_b),
      se = c(simple_slope_1_se, simple_slope_2_se),
      z = c(simple_slope_1_z, simple_slope_2_z),
      p = c(simple_slope_1_p, simple_slope_2_p))
    simple_slopes_table_rounded <-
      data.table::copy(simple_slopes_table)
    simple_slopes_table_rounded[, b := round(b, round_b)]
    simple_slopes_table_rounded[, se := round(se, round_se)]
    simple_slopes_table_rounded[, z := round(z, round_z)]
    simple_slopes_table_rounded[, p := kim::pretty_round_p_value(
      p, round_digits_after_decimal = round_p)]
    message("\nSimple Slope Estimates Associated With Focal Value(s):")
    print(simple_slopes_table_rounded)
    # prepare the message output for the simple slopes analysis
    simple_slopes_results_msg <- kim::p0(
      "\nA simple slopes analysis revealed that\n",
      "the simple slope estimated for {'", mod_name, "' = ",
      mod_lvl_1, "} was\nb = ",
      round(simple_slope_1_b, round_b), ", SE = ",
      round(simple_slope_1_se, round_se), ", z = ",
      round(simple_slope_1_z, round_z), ", ",
      kim::pretty_round_p_value(
        simple_slope_1_p, include_p_equals = TRUE),
      ", whereas\nthe simple slope estimated for {'", mod_name, "' = ",
      mod_lvl_2, "} was\nb = ",
      round(simple_slope_2_b, round_b), ", SE = ",
      round(simple_slope_2_se, round_se), ", z = ",
      round(simple_slope_2_z, round_z), ", ",
      kim::pretty_round_p_value(
        simple_slope_2_p, include_p_equals = TRUE), ".")
    if (length(unique(dt[["dv"]])) == 2) {
      simple_slopes_results_msg <-
        paste0(simple_slopes_results_msg, "\n")
    }
    message(simple_slopes_results_msg)
  } else if (kim::lenu(dt$mod) > 2) {
    # cases when mod is continuous
    if (is.null(focal_values)) {
      focal_values <- c(
        mean(dt$mod) - stats::sd(dt$mod),
        mean(dt$mod),
        mean(dt$mod) + stats::sd(dt$mod))
    }
    # conduct spotlight regressions
    spotlight_results <- lapply(seq_along(focal_values), function(i) {
      # create a column for mod minus the focal value of the iteration
      dt[, mod_minus_focal_value := mod - focal_values[i]]
      # simple slope associated with the focal value
      glm_3 <- NULL
      glm_3 <- stats::glm(
        formula = dv ~ iv * mod_minus_focal_value,
        data = dt, family = stats::binomial())
      glm_3_summary <- summary(glm_3)
      intercept_for_simple_slope_temp <- glm_3_summary[[
        "coefficients"]]["(Intercept)", "Estimate"]
      simple_slope_temp_b <- glm_3_summary[[
        "coefficients"]]["iv", "Estimate"]
      simple_slope_temp_se <- glm_3_summary[[
        "coefficients"]]["iv", "Std. Error"]
      simple_slope_temp_z <- glm_3_summary[[
        "coefficients"]]["iv", "z value"]
      simple_slope_temp_p <- glm_3_summary[[
        "coefficients"]]["iv", "Pr(>|z|)"]
      # output
      output <- data.table::data.table(
        focal_value_of_mod = focal_values[i],
        b = simple_slope_temp_b,
        se = simple_slope_temp_se,
        z = simple_slope_temp_z,
        p = simple_slope_temp_p)
      return(output)
    })
    simple_slopes_table <- data.table::rbindlist(spotlight_results)
    simple_slopes_table_rounded <-
      data.table::copy(simple_slopes_table)
    simple_slopes_table_rounded[, b := round(b, round_b)]
    simple_slopes_table_rounded[, se := round(se, round_se)]
    simple_slopes_table_rounded[, z := round(z, round_z)]
    simple_slopes_table_rounded[, p := kim::pretty_round_p_value(
      p, round_digits_after_decimal = round_p)]
    message("\nSimple Slope Estimates Associated With Focal Value(s):")
    print(simple_slopes_table_rounded)
    if (length(unique(dt[["dv"]])) == 2) {
      cat("\n")
    }
  }
}
