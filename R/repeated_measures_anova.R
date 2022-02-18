#' Repeated-Measures ANVOA
#'
#' Conduct a repeated-measures analysis of variance (ANOVA).
#' This analysis will be appropriate for within-subjects experimental design.
#'
#' The following package(s) must be installed prior to running the function:
#' Package 'ez' v4.4-0 (or possibly a higher version) by
#' Michael A Lawrence (2016),
#' <https://cran.r-project.org/package=ez>
#'
#' @param data a data object (a data frame or a data.table)
#' @param p_col_name name of the column identifying participants
#' @param measure_vars names of the columns containing repeated measures
#' (within-subjects variables)
#' @param round_w number of decimal places to which to round
#' W statistic from Mauchly's test (default = 2)
#' @param round_epsilon number of decimal places to which to round
#' the epsilon statistic from Greenhouse-Geisser or Huynh-Feldt
#' correction (default = 2)
#' @param round_df_model number of decimal places to which to round
#' the corrected degrees of freedom for model (default = 2)
#' @param round_df_error number of decimal places to which to round
#' the corrected degrees of freedom for error (default = 2)
#' @param round_f number of decimal places to which to round
#' the F statistic (default = 2)
#' @param round_ges number of decimal places to which to round
#' generalized eta-squared (default = 2)
#' @examples
#' repeated_measures_anova(
#'   data = mtcars, p_col_name = "cyl", measure_vars = c("wt", "qsec"))
#' @export
#' @import data.table
repeated_measures_anova <- function(
  data = NULL,
  p_col_name = NULL,
  measure_vars = NULL,
  round_w = 2,
  round_epsilon = 2,
  round_df_model = 2,
  round_df_error = 2,
  round_f = 2,
  round_ges = 2,
  output = NULL) {
  # installed packages
  installed_pkgs <- rownames(utils::installed.packages())
  # check if Package 'ez' is installed; this package is needed to run
  # the function ezanova
  if (!"ez" %in% installed_pkgs) {
    message(paste0(
      "This function requires the installation of Package 'ez'.",
      "\nTo install Package 'ez', type ",
      "'kim::prep(ez)'",
      "\n\nAlternatively, to install all packages (dependencies) required ",
      "for all\nfunctions in Package 'kim', type ",
      "'kim::install_all_dependencies()'"))
    return()
  } else {
    # proceed if Package 'ez' is already installed
    ezanova_from_ez <- utils::getFromNamespace(
      "ezANOVA", "ez")
  }
  # bind the vars locally to the function
  # convert data to data table
  dt1 <- data.table::setDT(data.table::copy(data))
  # convert to long format
  dt2 <- data.table::melt(
    dt1, id.vars = p_col_name, measure.vars = measure_vars)
  # change column names
  names(dt2) <- c("p", "within_subjects_vars", "value")
  # below, suppress the warning "converting x to factor for anova"
  anova_results <- suppressWarnings(ezanova_from_ez(
    data = dt2, dv = value, wid = p,
    within = within_subjects_vars, detailed = TRUE, type = 3))
  # anova table only
  at <- data.table::setDT(data.table::copy(anova_results[[1]]))
  # df
  df_model <- at[Effect == "within_subjects_vars", DFn]
  df_error <- at[Effect == "within_subjects_vars", DFd]
  # f stat
  f_stat <- at[Effect == "within_subjects_vars", F]
  f_stat_rounded <- sprintf(
    fmt = paste0("%.", round_f, "f"), round(
      f_stat, round_f))
  # p value
  f_p <- at[Effect == "within_subjects_vars", p]
  # effect text
  effect_text <- ifelse(
    f_p < 0.05, "significantly affected",
    "did not significantly affect")
  # effect size generalized eta squared
  generalized_eta_squared <- at[Effect == "within_subjects_vars", ges]
  generalized_eta_squared_rounded <- sprintf(
    fmt = paste0("%.", round_ges, "f"), round(
      generalized_eta_squared, round_ges))
  # effect size text
  es_text <- paste0(
    "generalized eta-squared = ", generalized_eta_squared_rounded)
  # proceed if there are at least 3 within subjects conditions
  if (length(unique(dt2[, within_subjects_vars])) >= 3) {
    # mauchly s test
    mauchly_w <- anova_results[[2]][["W"]]
    mauchly_p <- anova_results[[2]][["p"]]
    # check if sphericity is violated, ie mauchly test is significant
    if (mauchly_p < 0.05) {
      mauchly_text <- paste0(
        "Mauchly's test indicated that the assumption of phericity had",
        " been violated, W = ",
        round(mauchly_w, round_w), ", ",
        kim::pretty_round_p_value(mauchly_p, include_p_equals = TRUE),
        ".")
      # greenhouse geisser correction
      gg_p <- anova_results[[3]][["p[GG]"]]
      gg_e <- anova_results[[3]][["GGe"]]
      # huynh feldt correction
      hf_p <- anova_results[[3]][["p[HF]"]]
      hf_e <- anova_results[[3]][["HFe"]]
      # 4 possible cases
      if (gg_p < 0.05 & hf_p < 0.05) {
        correction_case <- 1
      } else if (gg_p > 0.05 & hf_p > 0.05) {
        correction_case <- 2
      } else if ((gg_p > 0.05 & hf_p < 0.05) | (gg_p < 0.05 & hf_p > 0.05)) {
        # following Field's suggestion, isbn: 978-1-4462-0045-2, p. 572
        average_of_p <- (gg_p + hf_p) / 2
        if (average_of_p > 0.05) {
          correction_case <- 1
        } else if (average_of_p < 0.05) {
          correction_case <- 2
        }
      } else {
        correction_case <- 99
      }
      # correction and p text by correction case
      if (correction_case == 1) {
        correction_text <- paste0(
          "Therefore Greenhouse-Geisser corrected tests are reported (",
          "epsilon = ",
          pretty_round_r(gg_e, round_epsilon),
          ").")
        df_model_corrected <- df_model * gg_e
        df_error_corrected <- df_error * gg_e
        # p value
        p_text <- kim::pretty_round_p_value(gg_p, include_p_equals = TRUE)
        # effect text
        effect_text <- ifelse(
          gg_p < 0.05, "significantly affected",
          "did not significantly affect")
      } else if (correction_case == 2) {
        correction_text <- paste0(
          "Therefore degrees of freedom were corrected using Huynh-Feldt",
          " estimates of sphericity (",
          "epsilon = ",
          pretty_round_r(hf_e, round_epsilon),
          ").")
        df_model_corrected <- df_model * hf_e
        df_error_corrected <- df_error * hf_e
        # p value
        p_text <- kim::pretty_round_p_value(hf_p, include_p_equals = TRUE)
        # effect text
        effect_text <- ifelse(
          hf_p < 0.05, "significantly affected",
          "did not significantly affect")
      } else if (correction_case == 99) {
        correction_text <-
          "[There seems to be an error with sphericity correction.]"
      }
      # f stat and p value by correction case
      if (correction_case %in% 1:2) {
        df_model_corrected_rounded <- sprintf(
          fmt = paste0("%.", round_df_model, "f"), round(
            df_model_corrected, round_df_model))
        df_error_corrected_rounded <- sprintf(
          fmt = paste0("%.", round_df_error, "f"), round(
            df_error_corrected, round_df_error))
        f_stat_text <- paste0(
          "F(", df_model_corrected_rounded, ", ",
          df_error_corrected_rounded, ") = ", f_stat_rounded)
      }
    } else {
      mauchly_text <- paste0(
        "Mauchly's test indicated that the assumption of sphericity had",
        " not been violated, W = ",
        round(mauchly_w, round_w), ", ",
        kim::pretty_round_p_value(mauchly_p, include_p_equals = TRUE),
        ".")
      correction_text <- ""
      # p value
      p_text <- kim::pretty_round_p_value(f_p, include_p_equals = TRUE)
      f_stat_text <- paste0(
        "F(", df_model, ", ",
        df_error, ") = ", f_stat_rounded)
    }
    # results text
    results_text <- paste0(
      "The results show that the within-subjects IV ",
      effect_text, " the DV")
    # results summary; combine texts
    results_summary <- paste0(
      mauchly_text, "\n", correction_text, "\n",
      results_text, ", ", f_stat_text, ", ",
      p_text, ", ", es_text, ".")
  } else {
    # there are only 2 within-subjects conditions
    # results text
    results_text <- paste0(
      "The results show that the within-subjects IV ",
      effect_text, " the DV")
    # f stat
    f_stat_text <- paste0(
      "F(", df_model, ", ",
      df_error, ") = ", f_stat_rounded)
    # p
    p_text <- kim::pretty_round_p_value(f_p, include_p_equals = TRUE)
    # results summary; combine texts
    results_summary <- paste0(
      results_text, ", ", f_stat_text, ", ",
      p_text, ", ", es_text, ".")
  }
  # output
  if (is.null(output)) {
    output <- anova_results
    output$results_summary <- results_summary
    print(anova_results)
    message(results_summary)
    invisible(output)
  }
}
