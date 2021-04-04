#' t test, pairwise
#'
#' Conducts a t-test for every possible pairwise comparison
#' with Bonferroni correction
#'
#' @param data a data object (a data frame or a data.table)
#' @param iv_name name of the independent variable
#' @param dv_name name of the dependent variable
#' @param sigfigs number of significant digits to round to
#' @param t_test_df_decimals number of decimals for the degrees of freedom
#' in t-tests (default = 1)
#' @param mann_whitney if \code{TRUE}, Mann-Whitney test results will be
#' included in the output data.table. If \code{TRUE}, Mann-Whitney
#' tests will not be performed.
#' @param t_test_stats if \code{t_test_stats = TRUE}, t-test statistic
#' and degrees of freedom will be included in the output data.table.
#' @param sd if \code{sd = TRUE}, standard deviations will be
#' included in the output data.table.
#' @return the output will be a data.table showing results of all
#' pairwise comparisons between levels of the independent variable.
#' @examples
#' t_test_pairwise(data = iris, iv_name = "Species", dv_name = "Sepal.Length")
#' t_test_pairwise(data = iris, iv_name = "Species",
#' dv_name = "Sepal.Length", t_test_stats = TRUE, sd = TRUE)
#' t_test_pairwise(data = iris, iv_name = "Species", dv_name = "Sepal.Length",
#' mann_whitney = FALSE)
#' @export
#' @import data.table
t_test_pairwise <- function(
  data = NULL,
  iv_name = NULL,
  dv_name = NULL,
  sigfigs = 3,
  mann_whitney = TRUE,
  t_test_stats = FALSE,
  t_test_df_decimals = 1,
  sd = FALSE) {
  # bind the vars locally to the function
  iv <- dv <- group_1 <- group_2 <- NULL
  # check number of iv_name and dv_name
  if (length(iv_name) > 1) {
    message(paste0(
      "The current version can handle only one independent variable.\n",
      "Please enter only one IV."))
  }
  # remove na
  dt01 <- setDT(copy(data))[, c(iv_name, dv_name), with = FALSE]
  names(dt01) <- c("iv", "dv")
  # convert iv to factor
  dt01[, iv := factor(iv)]
  dt01 <- stats::na.omit(dt01)
  # pairs
  group <- sort(unique(dt01$iv))
  dt02 <- data.table(t(utils::combn(group, 2)))
  names(dt02) <- c("group_1", "group_2")
  # group sizes
  group_sizes <- lapply(seq_len(nrow(dt02)), function(i) {
    group_1_n <- dt01[iv == dt02[, group_1[i]], .N]
    group_2_n <- dt01[iv == dt02[, group_2[i]], .N]
    output <- c(group_1_n, group_2_n)
    return(output)
  })
  group_sizes_dt <- as.data.table(do.call(rbind, group_sizes))
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
  cohen_d <- vapply(seq_len(nrow(dt02)), function(i) {
    temp <- dt01[iv %in% dt02[i, ]]
    temp[, iv := factor(iv)]
    kim::cohen_d_from_cohen_textbook(
      data = temp, iv_name = "iv", dv_name = "dv")},
    FUN.VALUE = numeric(1L))
  # t stat
  t_test_stat <- vapply(seq_len(nrow(dt02)), function(i) {
    stats::t.test(dv ~ iv, dt01[iv %in% dt02[i, ]])[["statistic"]]},
    FUN.VALUE = numeric(1L))
  # t stat df
  t_test_df <- vapply(seq_len(nrow(dt02)), function(i) {
    stats::t.test(dv ~ iv, dt01[iv %in% dt02[i, ]])[["parameter"]][["df"]]},
    FUN.VALUE = numeric(1L))
  # t test p values
  t_test_p_value <- vapply(seq_len(nrow(dt02)), function(i) {
    stats::t.test(dv ~ iv, dt01[iv %in% dt02[i, ]])[["p.value"]]},
    FUN.VALUE = numeric(1L))
  # bonferroni sig
  bonferroni_signif_for_t_test <- ifelse(
    t_test_p_value < .05 / nrow(dt02), "Yes", "No")
  # put everyhing together
  output <- data.table(
    dt02,
    group_sizes_dt,
    group_1_mean = signif(group_1_mean, sigfigs),
    group_2_mean = signif(group_2_mean, sigfigs))
  # add sd
  if (sd == TRUE) {
    output <- data.table(
      output,
      group_1_sd = signif(group_1_sd, sigfigs),
      group_2_sd = signif(group_2_sd, sigfigs))
  }
  # add t test stats
  if (t_test_stats == TRUE) {
    output <- data.table(
      output,
      t_test_df = round(t_test_df, t_test_df_decimals),
      t_test_stat = signif(t_test_stat, sigfigs),
      t_test_p_value = kim::pretty_round_p_value(t_test_p_value),
      bonferroni_signif_for_t_test = bonferroni_signif_for_t_test)
  }
  # add the rest
  output <- data.table(
    output,
    cohen_d = signif(cohen_d, sigfigs),
    t_test_p_value = kim::pretty_round_p_value(t_test_p_value),
    bonferroni_signif_for_t_test = bonferroni_signif_for_t_test)
  # mann whitney
  if (mann_whitney == TRUE) {
    mann_whitney_p_value <- vapply(seq_len(nrow(dt02)), function(i) {
      stats::wilcox.test(
        dv ~ iv, dt01[iv %in% dt02[i, ]],
        paired = FALSE)[["p.value"]]},
      FUN.VALUE = numeric(1L))
    # bonferroni sig
    bonferroni_signif_for_mann_whitney <- ifelse(
      mann_whitney_p_value < .05 / nrow(dt02), "Yes", "No")
    output <- data.table(
      output,
      mann_whitney_p_value = kim::pretty_round_p_value(
        mann_whitney_p_value),
      bonferroni_signif_for_mann_whitney)
  }
  return(output)
}
