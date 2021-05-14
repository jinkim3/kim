#' Chi square test, pairwise
#'
#' Conducts a chi-square test for every possible pairwise comparison
#' with Bonferroni correction
#'
#' @param data a data object (a data frame or a data.table)
#' @param iv_name name of the independent variable
#' (must be a categorical variable)
#' @param dv_name name of the dependent variable (must be a binary variable)
#' @param focal_dv_value focal value of the dependent variable
#' whose frequencies will be calculated (i.e., the value of the
#' dependent variable that will be considered a "success" or
#' a result of interest)
#' @param percent_and_total logical. If \code{percent_and_total = TRUE},
#' tabulate percentages of the focal DV value and a total count of
#' the two values in DV. By default \code{percent_and_total = FALSE}
#' @param percentages_only tabulate percentages of the focal DV value only
#' @param counts_only tabulate counts of the focal DV value only
#' @param sigfigs number of significant digits to round to
#' @param chi_sq_test_stats if \code{chi_sq_test_stats = TRUE},
#' chi-square test statistic and degrees of freedom will be included
#' in the pairwise comparison data.table.
#' @examples
#' chi_square_test_pairwise(data = mtcars, iv_name = "vs", dv_name = "am")
#' chi_square_test_pairwise(data = mtcars, iv_name = "vs", dv_name = "am",
#' percentages_only = TRUE)
#' # using 3 mtcars data sets combined
#' chi_square_test_pairwise(
#' data = rbind(mtcars, rbind(mtcars, mtcars)),
#' iv_name = "cyl", dv_name = "am")
#' chi_square_test_pairwise(
#' data = rbind(mtcars, rbind(mtcars, mtcars)),
#' iv_name = "cyl", dv_name = "am", percent_and_total = TRUE)
#' @export
chi_square_test_pairwise <- function(
  data = NULL,
  iv_name = NULL,
  dv_name = NULL,
  focal_dv_value = NULL,
  percent_and_total = FALSE,
  percentages_only = NULL,
  counts_only = NULL,
  sigfigs = 3,
  chi_sq_test_stats = FALSE
) {
  # bind the vars locally to the function
  iv <- dv <- g1_dv_count <- g2_dv_count <- NULL
  g1_dv_lvl_1_percent <- g1_dv_lvl_2_percent <-
    g2_dv_lvl_1_percent <- g2_dv_lvl_2_percent <- NULL
  # remove na
  dt01 <- data.table::setDT(data.table::copy(data))[, c(
    iv_name, dv_name), with = FALSE]
  names(dt01) <- c("iv", "dv")
  dt01 <- stats::na.omit(dt01)
  # make sure the dv has only two levels of value
  values_of_dv <- sort(unique(dt01$dv))
  if (length(values_of_dv) != 2) {
    stop(paste0(
      "The DV has ", length(values_of_dv), " level(s), rather than the ",
      "expected 2 levels."))
  }
  # set default focal dv value to be the latter of the two binary values
  if (is.null(focal_dv_value)) {
    focal_dv_value <- values_of_dv[2]
  }
  # pairs
  group <- sort(unique(dt01$iv))
  dt02 <- data.table::data.table(t(utils::combn(group, 2)))
  names(dt02) <- c("g1", "g2")
  # if percent and total option is true
  if (percent_and_total == TRUE) {
    # count of level 1 value of the dv (i.e., values_of_dv[1]) in group 1
    g1_dv_lvl_1_count <- vapply(seq_len(nrow(dt02)), function(i) {
      dt01[iv == dt02[["g1"]][i] & dv == values_of_dv[1], .N]
    }, numeric(1L))
    # count of level 2 value of the dv (i.e., values_of_dv[2]) in group 1
    g1_dv_lvl_2_count <- vapply(seq_len(nrow(dt02)), function(i) {
      dt01[iv == dt02[["g1"]][i] & dv == values_of_dv[2], .N]
    }, numeric(1L))
    # count of level 1 value of the dv (i.e., values_of_dv[1]) in group 2
    g2_dv_lvl_1_count <- vapply(seq_len(nrow(dt02)), function(i) {
      dt01[iv == dt02[["g2"]][i] & dv == values_of_dv[1], .N]
    }, numeric(1L))
    # count of level 2 value of the dv (i.e., values_of_dv[2]) in group 2
    g2_dv_lvl_2_count <- vapply(seq_len(nrow(dt02)), function(i) {
      dt01[iv == dt02[["g2"]][i] & dv == values_of_dv[2], .N]
    }, numeric(1L))
    # count data table
    dt03 <- data.table(
      g1_dv_lvl_1_count, g1_dv_lvl_2_count,
      g2_dv_lvl_1_count, g2_dv_lvl_2_count)
    # total counts
    dt03[, g1_dv_count := g1_dv_lvl_1_count + g1_dv_lvl_2_count]
    dt03[, g2_dv_count := g2_dv_lvl_1_count + g2_dv_lvl_2_count]
    # percentages
    dt03[, g1_dv_lvl_1_percent :=
           kim::round_flexibly(
             g1_dv_lvl_1_count / g1_dv_count * 100, sigfigs)]
    dt03[, g1_dv_lvl_2_percent :=
           kim::round_flexibly(
             g1_dv_lvl_2_count / g1_dv_count * 100, sigfigs)]
    dt03[, g2_dv_lvl_1_percent :=
           kim::round_flexibly(
             g2_dv_lvl_1_count / g2_dv_count * 100, sigfigs)]
    dt03[, g2_dv_lvl_2_percent :=
           kim::round_flexibly(
             g2_dv_lvl_2_count / g2_dv_count * 100, sigfigs)]
    # output based on focal value
    if (focal_dv_value == values_of_dv[1]) {
      dt04 <- dt03[, c(
        "g1_dv_lvl_1_percent", "g1_dv_count",
        "g2_dv_lvl_1_percent", "g2_dv_count"), with = FALSE]
      names(dt04) <- c(
        paste0("% of ", values_of_dv[1], " in g1"),
        paste0("count of ", values_of_dv[1], " and ",
               values_of_dv[2], " in g1"),
        paste0("% of ", values_of_dv[1], " in g2"),
        paste0("count of ", values_of_dv[1], " and ",
               values_of_dv[2], " in g2"))
    } else if (focal_dv_value == values_of_dv[2]) {
      dt04 <- dt03[, c(
        "g1_dv_lvl_2_percent", "g1_dv_count",
        "g2_dv_lvl_2_percent", "g2_dv_count"), with = FALSE]
      names(dt04) <- c(
        paste0("% of ", values_of_dv[2], " in g1"),
        paste0("count of ", values_of_dv[1], " and ",
               values_of_dv[2], " in g1"),
        paste0("% of ", values_of_dv[2], " in g2"),
        paste0("count of ", values_of_dv[1], " and ",
               values_of_dv[2], " in g2"))
    }
    # dt04 is section_2
    section_2 <- dt04
  } else {
    # counts and percentages for each group in each pair
    counts_1 <- data.table::setDT(
      lapply(values_of_dv, function(j) {
        vapply(dt02$g1, function(i) {
          nrow(dt01[iv == i & dv == j])}, FUN.VALUE = numeric(1L))}))
    percentages_1 <- kim::round_flexibly(data.table::setDT(
      lapply(seq_along(counts_1), function(i) {
        counts_1[[i]] / rowSums(counts_1) * 100})), sigfigs)
    counts_2 <- data.table::setDT(
      lapply(values_of_dv, function(j) {
        vapply(dt02$g2, function(i) {
          nrow(dt01[iv == i & dv == j])}, FUN.VALUE = numeric(1L))}))
    percentages_2 <- kim::round_flexibly(data.table::setDT(
      lapply(seq_along(counts_2), function(i) {
        counts_2[[i]] / rowSums(counts_2) * 100})), sigfigs)
    # set names
    names(counts_1) <- as.character(values_of_dv)
    names(percentages_1) <- as.character(values_of_dv)
    names(counts_2) <- as.character(values_of_dv)
    names(percentages_2) <- as.character(values_of_dv)
    # counts and percentages
    section_2 <- data.table::data.table(
      counts_1, percentages_1, counts_2, percentages_2)
    section_2 <- section_2[
      , names(section_2) == as.character(focal_dv_value), with = FALSE]
    names(section_2) <-
      paste0(c("count of ", "% of ", "count of ", "% of "),
             focal_dv_value,
             c(" in g1", " in g1", " in g2", " in g2"))
    if (!is.null(percentages_only)) {
      if (percentages_only == TRUE) {
        section_2 <- data.table::data.table(percentages_1, percentages_2)
        section_2 <- section_2[
          , names(section_2) == as.character(focal_dv_value), with = FALSE]
        names(section_2) <- paste0(
          "% of ", focal_dv_value, c(" in g1", " in g2"))
      }
    }
    if (!is.null(counts_only)) {
      if (counts_only == TRUE) {
        section_2 <- data.table::data.table(counts_1, counts_2)
        section_2 <- section_2[
          , names(section_2) == as.character(focal_dv_value), with = FALSE]
        names(section_2) <-
          paste0("count of ", focal_dv_value, c(" in g1", " in g2"))
      }
    }
  }
  # chi square test results
  chi_sq_test_results <- lapply(seq_len(nrow(dt02)), function(i) {
    dt03 <- dt01[iv %in% dt02[i, ]]
    stats::chisq.test(
      dt03$iv, dt03$dv, correct = FALSE)
  })
  # chi square test stats
  if (chi_sq_test_stats == TRUE) {
    # chi-square test df
    chi_sq_test_df <- vapply(seq_len(nrow(dt02)), function(i) {
      chi_sq_test_results[[i]][["parameter"]][["df"]]
    }, FUN.VALUE = numeric(1L))
    # chi-square test stat
    chi_sq_test_stat <- vapply(seq_len(nrow(dt02)), function(i) {
      chi_sq_test_results[[i]][["statistic"]]
    }, FUN.VALUE = numeric(1L))
  }
  # chi-square test p values
  chi_sq_p_value <- vapply(seq_len(nrow(dt02)), function(i) {
    chi_sq_test_results[[i]][["p.value"]]
  }, FUN.VALUE = numeric(1L))
  # bonferroni
  bonferroni_sig <- ifelse(
    chi_sq_p_value < .05 / nrow(dt02), "Yes", "No")
  # output
  output <- data.table::data.table(
    dt02, section_2, chi_sq_p_value =
      kim::pretty_round_p_value(chi_sq_p_value),
    bonferroni_sig)
  # add chi square test stats
  if (chi_sq_test_stats == TRUE) {
    output <- data.table::data.table(
      dt02, section_2,
      chi_sq_test_df,
      chi_sq_test_stat = kim::round_flexibly(chi_sq_test_stat, sigfigs),
      chi_sq_p_value = kim::pretty_round_p_value(chi_sq_p_value),
      bonferroni_sig)
  }
  return(output)
}
