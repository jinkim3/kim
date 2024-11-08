#' t-tests, pairwise
#'
#' Conducts a t-test for every possible pairwise comparison
#' with Holm or Bonferroni correction
#'
#' @param data a data object (a data frame or a data.table)
#' @param iv_name name of the independent variable
#' @param dv_name name of the dependent variable
#' @param sigfigs number of significant digits to round to
#' @param welch Should Welch's t-tests be conducted?
#' By default, \code{welch = TRUE}
#' @param cohen_d if \code{cohen_d = TRUE}, Cohen's d statistics will be
#' included in the output data.table.
#' @param cohen_d_w_ci if \code{cohen_d_w_ci = TRUE},
#' Cohen's d with 95% CI will be included in the output data.table.
#' @param adjust_p the name of the method to use to adjust p-values.
#' If \code{adjust_p = "holm"}, the Holm method will be used;
#' if \code{adjust_p = "bonferroni"}, the Bonferroni method will be used.
#' By default, \code{adjust_p = "holm"}
#' @param bonferroni The use of this argument is deprecated.
#' Use the 'adjust_p' argument instead.
#' If \code{bonferroni = TRUE}, Bonferroni tests will be
#' conducted for t-tests or Mann-Whitney tests.
#' @param mann_whitney if \code{TRUE}, Mann-Whitney test results will be
#' included in the output data.table. If \code{FALSE}, Mann-Whitney
#' tests will not be performed.
#' @param mann_whitney_exact this is the input for the 'exact'
#' argument used in the 'stats::wilcox.test' function, which
#' conducts a Mann-Whitney test. By default, \code{
#' mann_whitney_exact = FALSE}. If you want to use the default settings
#' for the 'stats::wilcox.test' function, consider setting
#' \code{mann_whitney_exact = TRUE}.
#' @param t_test_stats if \code{t_test_stats = TRUE}, t-test statistic
#' and degrees of freedom will be included in the output data.table.
#' By default, \code{t_test_stats = TRUE}
#' @param sd if \code{sd = TRUE}, standard deviations will be
#' included in the output data.table.
#' @param round_p number of decimal places to which to round
#' p-values (default = 3)
#' @param anova Should a one-way ANOVA be conducted and reported?
#' By default, \code{anova = FALSE}, but when there are more than two
#' levels in the independent variable, the value will change such tat
#' \code{anova = TRUE}.
#' @param round_f number of decimal places to which to round
#' the f statistic (default = 2)
#' @param round_t number of decimal places to which to round
#' the t statistic (default = 2)
#' @param round_t_test_df number of decimal places to which to round
#' the degrees of freedom for t tests (default = 2)
#' @return the output will be a data.table showing results of all
#' pairwise comparisons between levels of the independent variable.
#' @examples
#' \dontrun{
#' # Basic example
#' t_test_pairwise(
#' data = iris, iv_name = "Species", dv_name = "Sepal.Length")
#' # Welch's t-test
#' t_test_pairwise(
#' data = mtcars, iv_name = "am", dv_name = "hp")
#' # A Student's t-test
#' t_test_pairwise(
#' data = mtcars, iv_name = "am", dv_name = "hp", welch = FALSE)
#' # Other examples
#' t_test_pairwise(data = iris, iv_name = "Species",
#' dv_name = "Sepal.Length", t_test_stats = TRUE, sd = TRUE)
#' t_test_pairwise(
#' data = iris, iv_name = "Species", dv_name = "Sepal.Length",
#' mann_whitney = FALSE)
#' }
#' @export
#' @import data.table
t_test_pairwise <- function(
  data = NULL,
  iv_name = NULL,
  dv_name = NULL,
  sigfigs = 3,
  welch = TRUE,
  cohen_d = TRUE,
  cohen_d_w_ci = TRUE,
  adjust_p = "holm",
  bonferroni = NULL,
  mann_whitney = TRUE,
  mann_whitney_exact = FALSE,
  t_test_stats = TRUE,
  sd = FALSE,
  round_p = 3,
  anova = FALSE,
  round_f = 2,
  round_t = 2,
  round_t_test_df = 2) {
  # bind the vars locally to the function
  iv <- dv <- group_1 <- group_2 <- NULL
  # check number of iv_name and dv_name
  if (length(iv_name) > 1) {
    message(paste0(
      "The current version can handle only one independent variable.\n",
      "Please enter only one IV."))
  }
  # remove na
  dt01 <- data.table::setDT(data.table::copy(
    data))[, c(iv_name, dv_name), with = FALSE]
  names(dt01) <- c("iv", "dv")
  # convert iv to factor
  dt01[, iv := factor(iv)]
  dt01 <- stats::na.omit(dt01)
  if (length(unique(dt01[, iv])) > 2) {
    anova <- TRUE
  }
  # conduct and report a one way anova
  if (anova == TRUE) {
    anova_results <- stats::aov(dv ~ iv, data = dt01)
    # alternatively called: Degree of Freedom for Groups,
    # df groups, df between
    anova_df_groups <- summary(anova_results)[[1]]["iv", "Df"]
    # alternatively called: Degree of Freedom for Error,
    # df error, df within, df residual
    anova_df_error <- summary(anova_results)[[1]]["Residuals", "Df"]
    anova_f <- summary(anova_results)[[1]]["iv", "F value"]
    anova_p <- summary(anova_results)[[1]]["iv", "Pr(>F)"]
    results <- paste0(
      "One-way ANOVA revealed that '", dv_name,
      "'\nvaried significantly as a function of '", iv_name,
      ",'\nF(", anova_df_groups, ", ", anova_df_error, ") = ",
      round(anova_f, round_f), ", ",
      kim::pretty_round_p_value(
        p_value_vector = anova_p,
        round_digits_after_decimal = round_p,
        include_p_equals = TRUE), ".\n")
    message(results)
  }
  # pairs
  group <- sort(unique(dt01$iv))
  dt02 <- data.table::data.table(t(utils::combn(group, 2)))
  names(dt02) <- c("group_1", "group_2")
  # group sizes
  group_sizes <- lapply(seq_len(nrow(dt02)), function(i) {
    group_1_n <- dt01[iv == dt02[, group_1[i]], .N]
    group_2_n <- dt01[iv == dt02[, group_2[i]], .N]
    output <- c(group_1_n, group_2_n)
    return(output)
  })
  group_sizes_dt <- data.table::as.data.table(do.call(rbind, group_sizes))
  names(group_sizes_dt) <- c("group_1_n", "group_2_n")
  # group means
  group_1_mean <-
    vapply(dt02[["group_1"]], function(i) {
      mean(dt01[iv == i]$dv, na.rm = TRUE)},
      FUN.VALUE = numeric(1L))
  group_2_mean <-
    vapply(dt02[["group_2"]], function(i) {
      mean(dt01[iv == i]$dv, na.rm = TRUE)},
           FUN.VALUE = numeric(1L))
  # sd
  if (sd == TRUE) {
    group_1_sd <-
      vapply(dt02[["group_1"]], function(i) {
        sd(dt01[iv == i]$dv, na.rm = TRUE)},
        FUN.VALUE = numeric(1L))
    group_2_sd <-
      vapply(dt02[["group_2"]], function(i) {
        sd(dt01[iv == i]$dv, na.rm = TRUE)},
        FUN.VALUE = numeric(1L))
  }
  # cohen d
  if (cohen_d == TRUE) {
    cohen_d_stat <- vapply(seq_len(nrow(dt02)), function(i) {
      temp <- dt01[iv %in% dt02[i, ]]
      temp[, iv := factor(iv)]
      kim::cohen_d_from_cohen_textbook(
        data = temp, iv_name = "iv", dv_name = "dv")},
      FUN.VALUE = numeric(1L))
  }
  # cohen d with ci
  if (cohen_d_w_ci == TRUE) {
    cohen_d_w_ci_result <- vapply(seq_len(nrow(dt02)), function(i) {
      temp <- dt01[iv %in% dt02[i, ]]
      temp[, iv := factor(iv)]
      cohen_d_and_ci <- kim::round_flexibly(kim::cohen_d(
        data = temp, iv_name = "iv", dv_name = "dv"))
      output <- paste0(
        cohen_d_and_ci[["cohen_d"]], " [",
        cohen_d_and_ci[["ci_95_ll"]], ", ",
        cohen_d_and_ci[["ci_95_ul"]], "]")
      return(output)
    }, FUN.VALUE = as.character(1L))
  }
  # welch s t tests
  treat_vars_as_equal <- ifelse(welch == TRUE, FALSE, TRUE)
  # t test results
  t_test_results <- lapply(seq_len(nrow(dt02)), function(i) {
    stats::t.test(
      formula = dv ~ iv,
      data = dt01[iv %in% dt02[i, ]],
      var.equal = treat_vars_as_equal)})
  # add t stats
  if (t_test_stats == TRUE) {
    # t stat
    t_test_stat <- sapply(t_test_results, function(test) test$statistic)
    # t stat df
    t_test_df <- sapply(
      t_test_results, function(test) test$parameter[["df"]])
  }
  # t test p values
  t_test_p_value <- sapply(
    t_test_results, function(test) test$p.value)
  # put everything together
  output <- data.table::data.table(
    dt02,
    group_sizes_dt,
    group_1_mean = kim::round_flexibly(group_1_mean, sigfigs),
    group_2_mean = kim::round_flexibly(group_2_mean, sigfigs))
  # add sd
  if (sd == TRUE) {
    output <- data.table::data.table(
      output,
      group_1_sd = kim::round_flexibly(group_1_sd, sigfigs),
      group_2_sd = kim::round_flexibly(group_2_sd, sigfigs))
  }
  # add cohen d
  if (cohen_d == TRUE) {
    output <- data.table::data.table(
      output,
      cohen_d = kim::round_flexibly(cohen_d_stat, sigfigs))
  }
  # add cohen d w ci
  if (cohen_d_w_ci == TRUE) {
    output <- data.table::data.table(
      output,
      cohen_d_w_95_ci = cohen_d_w_ci_result)
  }
  # add t test stats
  if (t_test_stats == TRUE) {
    output <- data.table::data.table(
      output,
      t_test_df = round(t_test_df, round_t_test_df),
      t_test_stat = kim::round_flexibly(t_test_stat, round_t))
  }
  # t test p values are added by default
  output[, "t_test_p_value" := kim::pretty_round_p_value(
    p_value_vector = t_test_p_value,
    round_digits_after_decimal = round_p)][]
  # report a t-test in the case where the IV has only two levels
  if (anova == FALSE) {
    results_first_part <- data.table::fcase(
      welch == TRUE, "A Welch's t-test",
      welch == FALSE, "A t-test")
    results <- paste0(
      results_first_part, " revealed that '", dv_name,
      "'\nvaried significantly as a function of '", iv_name,
      "',\nt(", round(t_test_df, round_t_test_df), ") = ",
      round(t_test_stat, round_t), ", ",
      kim::pretty_round_p_value(
        p_value_vector = t_test_p_value,
        round_digits_after_decimal = round_p,
        include_p_equals = TRUE), ".\n")
    message(results)
  }

  # mann whitney
  if (mann_whitney == TRUE) {
    mann_whitney_p_value <- vapply(seq_len(nrow(dt02)), function(i) {
      stats::wilcox.test(
        formula = dv ~ iv,
        data = dt01[iv %in% dt02[i, ]],
        exact = mann_whitney_exact)[["p.value"]]},
      FUN.VALUE = numeric(1L))
    output[, "mann_whitney_p_value" := kim::pretty_round_p_value(
      p_value_vector = mann_whitney_p_value,
      round_digits_after_decimal = round_p)][]
  }
  # adjust p values
  if (!is.null(bonferroni)) {
    if (bonferroni == TRUE) {
      adjust_p <- "bonferroni"
      warning(paste0(
        "The use of the 'bonferroni' argument is deprecated.\n",
        "Use the 'adjust_p' argument instead ",
        '(e.g., adjust_p = "bonferroni").'))
    }
  }
  # adjust p values with bonferroni
  if (length(t_test_p_value) > 1) {
    if (adjust_p == "holm") {
      holm_adjusted_p_for_t_tests <- kim::pretty_round_p_value(
        stats::p.adjust(p = t_test_p_value, method = "holm"),
        round_digits_after_decimal = round_p)
      output <- data.table::data.table(output, holm_adjusted_p_for_t_tests)
      if (mann_whitney == TRUE) {
        holm_adjusted_p_for_mann_whitney <- kim::pretty_round_p_value(
          stats::p.adjust(
            p = mann_whitney_p_value, method = "holm"),
          round_digits_after_decimal = round_p)
        output <- data.table::data.table(
          output, holm_adjusted_p_for_mann_whitney)
      }
    } else if (adjust_p == "bonferroni") {
      bonferroni_adjusted_p_for_t_tests <- kim::pretty_round_p_value(
        stats::p.adjust(p = t_test_p_value, method = "bonferroni"),
        round_digits_after_decimal = round_p)
      output <- data.table::data.table(
        output, bonferroni_adjusted_p_for_t_tests)
      if (mann_whitney == TRUE) {
        bonferroni_adjusted_p_for_mann_whitney <-
          kim::pretty_round_p_value(stats::p.adjust(
            p = mann_whitney_p_value, method = "bonferroni"),
            round_digits_after_decimal = round_p)
        output <- data.table::data.table(
          output, bonferroni_adjusted_p_for_mann_whitney)
      }
    }
  }
  return(output)
}
