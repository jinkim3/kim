#' t-tests, pairwise
#'
#' Conducts a t-test for every possible pairwise comparison
#' with Holm or Bonferroni correction
#'
#' @param data a data object (a data frame or a data.table)
#' @param iv_name name of the independent variable
#' @param dv_name name of the dependent variable
#' @param sigfigs number of significant digits to round to
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
#' @param t_test_stats if \code{t_test_stats = TRUE}, t-test statistic
#' and degrees of freedom will be included in the output data.table.
#' @param t_test_df_decimals number of decimals for the degrees of freedom
#' in t-tests (default = 1)
#' @param sd if \code{sd = TRUE}, standard deviations will be
#' included in the output data.table.
#' @param round_p number of decimal places to which to round
#' p-values (default = 3)
#' @return the output will be a data.table showing results of all
#' pairwise comparisons between levels of the independent variable.
#' @examples
#' \dontrun{
#' t_test_pairwise(data = iris, iv_name = "Species", dv_name = "Sepal.Length")
#' t_test_pairwise(data = iris, iv_name = "Species",
#' dv_name = "Sepal.Length", t_test_stats = TRUE, sd = TRUE)
#' t_test_pairwise(data = iris, iv_name = "Species", dv_name = "Sepal.Length",
#' mann_whitney = FALSE)
#' }
#' @export
#' @import data.table
t_test_pairwise <- function(
  data = NULL,
  iv_name = NULL,
  dv_name = NULL,
  sigfigs = 3,
  cohen_d = TRUE,
  cohen_d_w_ci = TRUE,
  adjust_p = "holm",
  bonferroni = NULL,
  mann_whitney = TRUE,
  t_test_stats = FALSE,
  t_test_df_decimals = 1,
  sd = FALSE,
  round_p = 3) {
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
  # t stats?
  if (t_test_stats == TRUE) {
    # t stat
    t_test_stat <- vapply(seq_len(nrow(dt02)), function(i) {
      stats::t.test(dv ~ iv, dt01[iv %in% dt02[i, ]])[["statistic"]]},
      FUN.VALUE = numeric(1L))
    # t stat df
    t_test_df <- vapply(seq_len(nrow(dt02)), function(i) {
      stats::t.test(
        dv ~ iv, dt01[iv %in% dt02[i, ]])[["parameter"]][["df"]]},
      FUN.VALUE = numeric(1L))
  }
  # t test p values
  t_test_p_value <- vapply(seq_len(nrow(dt02)), function(i) {
    stats::t.test(dv ~ iv, dt01[iv %in% dt02[i, ]])[["p.value"]]},
    FUN.VALUE = numeric(1L))
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
      t_test_df = round(t_test_df, t_test_df_decimals),
      t_test_stat = kim::round_flexibly(t_test_stat, sigfigs))
  }
  # t test p values are added by default
  output[, "t_test_p_value" := kim::pretty_round_p_value(
    p_value_vector = t_test_p_value,
    round_digits_after_decimal = round_p)][]
  # mann whitney
  if (mann_whitney == TRUE) {
    mann_whitney_p_value <- vapply(seq_len(nrow(dt02)), function(i) {
      stats::wilcox.test(
        dv ~ iv, dt01[iv %in% dt02[i, ]],
        paired = FALSE)[["p.value"]]},
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
  if (adjust_p == "holm") {
    holm_adjusted_p_for_t_tests <- kim::pretty_round_p_value(
      stats::p.adjust(p = t_test_p_value, method = "holm"))
    output <- data.table::data.table(output, holm_adjusted_p_for_t_tests)
    if (mann_whitney == TRUE) {
      holm_adjusted_p_for_mann_whitney <- kim::pretty_round_p_value(
        stats::p.adjust(
          p = mann_whitney_p_value, method = "holm"))
      output <- data.table::data.table(
        output, holm_adjusted_p_for_mann_whitney)
    }
  } else if (adjust_p == "bonferroni") {
    bonferroni_adjusted_p_for_t_tests <- kim::pretty_round_p_value(
      stats::p.adjust(p = t_test_p_value, method = "bonferroni"))
    output <- data.table::data.table(
      output, bonferroni_adjusted_p_for_t_tests)
    if (mann_whitney == TRUE) {
      bonferroni_adjusted_p_for_mann_whitney <- kim::pretty_round_p_value(
        stats::p.adjust(
          p = mann_whitney_p_value, method = "bonferroni"))
      output <- data.table::data.table(
        output, bonferroni_adjusted_p_for_mann_whitney)
    }
  }
  return(output)
}
