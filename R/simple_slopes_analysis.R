#' Simple slopes analysis
#'
#' Conduct a simple slopes analysis, typically to probe a two-way
#' interaction.
#'
#' @param data a data object (a data frame or a data.table)
#' @param iv_name name of the independent variable (IV)
#' @param dv_name name of the dependent variable (DV)
#' @param mod_name name of the moderator variable (MOD)
#' @param focal_values this input will be used only in cases where IV
#' is continuous. In such cases, what are the focal values of the IV
#' at which to estimate the effect of MOD on DV? By default, values
#' corresponding to the mean of IV +/-1 SD will be used.
#' @param round_b number of decimal places to which to round
#' coefficients from the regression analysis (default = 2)
#' @param round_se number of decimal places to which to round
#' standard error values from the regression analysis (default = 2)
#' @param round_t number of decimal places to which to round
#' t statistics from the regression analysis (default = 2)
#' @param round_p number of decimal places to which to round
#' p values from the regression analysis (default = 2)
#' @examples
#' \donttest{
#' simple_slopes_analysis(
#' data = mtcars, iv_name = "vs", dv_name = "mpg", mod_name = "hp")
#' simple_slopes_analysis(
#' data = mtcars, iv_name = "disp", dv_name = "mpg", mod_name = "hp")
#' simple_slopes_analysis(
#' data = mtcars, iv_name = "vs", dv_name = "am", mod_name = "hp")
#' simple_slopes_analysis(
#' data = mtcars, iv_name = "disp", dv_name = "am", mod_name = "hp")
#' }
#' @export
#' @import data.table
simple_slopes_analysis <- function(
    data = NULL,
    iv_name = NULL,
    dv_name = NULL,
    mod_name = NULL,
    round_b = 2,
    round_se = 2,
    round_t = 2,
    round_p = 3,
    focal_values = NULL
) {
  # bind the vars locally to the function
  iv <- dv <- mod <- iv_binary_1 <- iv_binary_2 <-
    b <- se <- p <- iv_minus_focal_value <- NULL
  # take a subset of data
  dt <- data.table::setDT(data.table::copy(data))
  dt <- dt[, c(iv_name, mod_name, dv_name), with = FALSE]
  # deal with na values
  dt <- stats::na.omit(dt)
  # check if dv is binary
  if (length(unique(dt[[dv_name]])) == 2) {
    warning(paste0(
      "\nThe DV seems to be a binary variable.\n",
      "To conduct a simple slopes analysis with ",
      "logistic regression analyses,\nuse the function '",
      "simple_slopes_analysis_logistic'."))
  }
  # formula for the interaction analysis
  lm_formula_for_interaction <- stats::as.formula(paste0(
    dv_name, " ~ ", iv_name, " * ", mod_name))
  # interaction analysis
  print(kim::multiple_regression(
    data = dt,
    formula = lm_formula_for_interaction))
  lm_interaction <- stats::lm(
    formula = lm_formula_for_interaction,
    data = dt)
  lm_interaction_summary <- summary(lm_interaction)
  # name of the interaction term in the model
  reg_table_row_names <- row.names(
    lm_interaction_summary[["coefficients"]])
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
  interaction_b <- lm_interaction_summary[[
    "coefficients"]][interaction_term_name, "Estimate"]
  interaction_se <- lm_interaction_summary[[
    "coefficients"]][interaction_term_name, "Std. Error"]
  interaction_t <- lm_interaction_summary[[
    "coefficients"]][interaction_term_name, "t value"]
  interaction_p <- lm_interaction_summary[[
    "coefficients"]][interaction_term_name, "Pr(>|t|)"]
  results_msg_interaction_sig_text <- data.table::fcase(
    interaction_p < 0.05, "significantly predicted",
    interaction_p >= 0.05, "did not significantly predict",
    default = "[error: interaction_p was not properly calculated]")
  # df associated with the residual aka the error degrees of freedom
  error_df <- stats::df.residual(lm_interaction)
  # prepare the message output for the interaction analysis
  interaction_results_msg <- kim::p0(
    "\nA multiple regression analysis revealed that\n",
    "the interaction between '", iv_name, "' and '", mod_name, "'\n",
    results_msg_interaction_sig_text, " '", dv_name, "',\nb = ",
    round(interaction_b, round_b), ", SE = ",
    round(interaction_se, round_se), ", t(", error_df,
    ") = ", round(interaction_t, round_t), ", ",
    kim::pretty_round_p_value(
      interaction_p, include_p_equals = TRUE))
  message(interaction_results_msg)
  # change var names
  names(dt) <- c("iv", "mod", "dv")
  # cases when iv is binary
  if (kim::lenu(dt$iv) == 2) {
    iv_lvl_1 <- sort(unique(dt$iv))[1]
    iv_lvl_2 <- sort(unique(dt$iv))[2]
    dt[, iv_binary_1 := data.table::fcase(
      iv == iv_lvl_1, 0,
      iv == iv_lvl_2, 1)]
    dt[, iv_binary_2 := data.table::fcase(
      iv == iv_lvl_2, 0,
      iv == iv_lvl_1, 1)]
    # simple slope 1
    lm_1 <- stats::lm(
      formula = dv ~ iv_binary_1 * mod,
      data = dt)
    lm_1_summary <- summary(lm_1)
    intercept_for_simple_slope_1 <- lm_1_summary[[
      "coefficients"]]["(Intercept)", "Estimate"]
    simple_slope_1_b <- lm_1_summary[["coefficients"]]["mod", "Estimate"]
    simple_slope_1_se <- lm_1_summary[["coefficients"]]["mod", "Std. Error"]
    simple_slope_1_t <- lm_1_summary[["coefficients"]]["mod", "t value"]
    simple_slope_1_p <- lm_1_summary[["coefficients"]]["mod", "Pr(>|t|)"]
    # simple slope 2
    lm_2 <- stats::lm(
      formula = dv ~ iv_binary_2 * mod,
      data = dt)
    lm_2_summary <- summary(lm_2)
    intercept_for_simple_slope_2 <- lm_2_summary[[
      "coefficients"]]["(Intercept)", "Estimate"]
    simple_slope_2_b <- lm_2_summary[["coefficients"]]["mod", "Estimate"]
    simple_slope_2_se <- lm_2_summary[["coefficients"]]["mod", "Std. Error"]
    simple_slope_2_t <- lm_2_summary[["coefficients"]]["mod", "t value"]
    simple_slope_2_p <- lm_2_summary[["coefficients"]]["mod", "Pr(>|t|)"]
    # output
    simple_slopes_table <- data.table::data.table(
      focal_value_of_iv = c(iv_lvl_1, iv_lvl_2),
      b = c(simple_slope_1_b, simple_slope_2_b),
      se = c(simple_slope_1_se, simple_slope_2_se),
      t = c(simple_slope_1_t, simple_slope_2_t),
      p = c(simple_slope_1_p, simple_slope_2_p))
    simple_slopes_table_rounded <-
      data.table::copy(simple_slopes_table)
    simple_slopes_table_rounded[, b := round(b, round_b)]
    simple_slopes_table_rounded[, se := round(se, round_se)]
    simple_slopes_table_rounded[, t := round(t, round_t)]
    simple_slopes_table_rounded[, p := kim::pretty_round_p_value(
      p, round_digits_after_decimal = round_p)]
    message("\nSimple Slope Estimates Associated With Focal Value(s):")
    print(simple_slopes_table_rounded)
    # prepare the message output for the simple slopes analysis
    simple_slopes_results_msg <- kim::p0(
      "\nA simple slopes analysis revealed that\n",
      "the simple slope estimated for {'", iv_name, "' = ",
      iv_lvl_1, "} was\nb = ",
      round(simple_slope_1_b, round_b), ", SE = ",
      round(simple_slope_1_se, round_se), ", t(", error_df,
      ") = ", round(simple_slope_1_t, round_t), ", ",
      kim::pretty_round_p_value(
        simple_slope_1_p, include_p_equals = TRUE),
      ", whereas\nthe simple slope estimated for {'", iv_name, "' = ",
      iv_lvl_2, "} was\nb = ",
      round(simple_slope_2_b, round_b), ", SE = ",
      round(simple_slope_2_se, round_se), ", t(", error_df,
      ") = ", round(simple_slope_2_t, round_t), ", ",
      kim::pretty_round_p_value(
        simple_slope_2_p, include_p_equals = TRUE), ".")
    if (length(unique(dt[["dv"]])) == 2) {
      simple_slopes_results_msg <-
        paste0(simple_slopes_results_msg, "\n")
    }
    message(simple_slopes_results_msg)
  } else if (kim::lenu(dt$iv) > 2) {
    # cases when iv is continuous
    if (is.null(focal_values)) {
      focal_values <- c(
        mean(dt$iv) - stats::sd(dt$iv),
        mean(dt$iv),
        mean(dt$iv) + stats::sd(dt$iv))
    }
    # conduct spotlight regressions
    spotlight_results <- lapply(seq_along(focal_values), function(i) {
      # create a column for iv minus the focal value of the iteration
      dt[, iv_minus_focal_value := iv - focal_values[i]]
      # simple slope associated with the focal value
      lm_3 <- NULL
      lm_3 <- stats::lm(
        formula = dv ~ iv_minus_focal_value * mod,
        data = dt)
      lm_3_summary <- summary(lm_3)
      intercept_for_simple_slope_temp <- lm_3_summary[[
        "coefficients"]]["(Intercept)", "Estimate"]
      simple_slope_temp_b <- lm_3_summary[[
        "coefficients"]]["mod", "Estimate"]
      simple_slope_temp_se <- lm_3_summary[[
        "coefficients"]]["mod", "Std. Error"]
      simple_slope_temp_t <- lm_3_summary[[
        "coefficients"]]["mod", "t value"]
      simple_slope_temp_p <- lm_3_summary[[
        "coefficients"]]["mod", "Pr(>|t|)"]
      # output
      output <- data.table::data.table(
        focal_value_of_iv = focal_values[i],
        b = simple_slope_temp_b,
        se = simple_slope_temp_se,
        t = simple_slope_temp_t,
        p = simple_slope_temp_p)
      return(output)
    })
    simple_slopes_table <- data.table::rbindlist(spotlight_results)
    simple_slopes_table_rounded <-
      data.table::copy(simple_slopes_table)
    simple_slopes_table_rounded[, b := round(b, round_b)]
    simple_slopes_table_rounded[, se := round(se, round_se)]
    simple_slopes_table_rounded[, t := round(t, round_t)]
    simple_slopes_table_rounded[, p := kim::pretty_round_p_value(
      p, round_digits_after_decimal = round_p)]
    message("\nSimple Slope Estimates Associated With Focal Value(s):")
    print(simple_slopes_table_rounded)
    if (length(unique(dt[["dv"]])) == 2) {
      cat("\n")
    }
  }
}
