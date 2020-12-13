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
#' @param percentages_only tabulate percentages of the focal DV value only
#' @param counts_only tabulate counts of the focal DV value only
#' @param sigfigs number of significant digits to round to
#' @examples
#' chi_square_test_pairwise(data = mtcars, iv_name = "vs", dv_name = "am")
#' chi_square_test_pairwise(data = mtcars, iv_name = "vs", dv_name = "am",
#' percentages_only = TRUE)
#' @export
chi_square_test_pairwise <- function(
  data = NULL,
  iv_name = NULL,
  dv_name = NULL,
  focal_dv_value = NULL,
  percentages_only = NULL,
  counts_only = NULL,
  sigfigs = 3
) {
  # bind the vars locally to the function
  iv <- dv <- NULL
  # remove na
  dt01 <- data.table::setDT(data.table::copy(data))[
    , c(iv_name, dv_name), with = FALSE]
  names(dt01) <- c("iv", "dv")
  dt01 <- stats::na.omit(dt01)
  # make sure the dv has only two levels of value
  values_of_dv <- sort(unique(dt01$dv))
  # pairs
  group <- sort(unique(dt01$iv))
  dt02 <- data.table::data.table(t(utils::combn(group, 2)))
  names(dt02) <- c("grp_1", "grp_2")
  # counts and percentages for each group in each pair
  counts_1 <- data.table::setDT(
    lapply(values_of_dv, function(j) {
      vapply(dt02$grp_1, function(i) {
        nrow(dt01[iv == i & dv == j])}, FUN.VALUE = numeric(1L))}))
  percentages_1 <- signif(data.table::setDT(
    lapply(seq_along(counts_1), function(i) {
    counts_1[[i]] / rowSums(counts_1) * 100})), sigfigs)
  counts_2 <- data.table::setDT(
    lapply(values_of_dv, function(j) {
      vapply(dt02$grp_2, function(i) {
        nrow(dt01[iv == i & dv == j])}, FUN.VALUE = numeric(1L))}))
  percentages_2 <- signif(data.table::setDT(
    lapply(seq_along(counts_2), function(i) {
      counts_2[[i]] / rowSums(counts_2) * 100})), sigfigs)
  # set names
  names(counts_1) <- as.character(values_of_dv)
  names(percentages_1) <- as.character(values_of_dv)
  names(counts_2) <- as.character(values_of_dv)
  names(percentages_2) <- as.character(values_of_dv)
  # set default focal dv value to be the latter of the two binary values
  if (is.null(focal_dv_value)) {
    focal_dv_value <- values_of_dv[2]
  }
  # counts and percentages
  section_2 <- data.table::data.table(
    counts_1, percentages_1, counts_2, percentages_2)
  section_2 <- section_2[
    , names(section_2) == as.character(focal_dv_value), with = FALSE]
  names(section_2) <-
    paste0(c("grp_1, count of ",
      "grp_1, % of ",
      "grp_2, count of ",
      "grp_2, % of "), focal_dv_value)
  if (!is.null(percentages_only)) {
    if (percentages_only == TRUE) {
      section_2 <- data.table::data.table(percentages_1, percentages_2)
      section_2 <- section_2[
        , names(section_2) == as.character(focal_dv_value), with = FALSE]
      names(section_2) <-
        paste0(c("grp_1, % of ", "grp_2, % of "), focal_dv_value)
    }
  }
  if (!is.null(counts_only)) {
    if (counts_only == TRUE) {
      section_2 <- data.table::data.table(counts_1, counts_2)
      section_2 <- section_2[
        , names(section_2) == as.character(focal_dv_value), with = FALSE]
      names(section_2) <-
        paste0(c("grp_1, count of ", "grp_2, count of "), focal_dv_value)
    }
  }
  # chi-square test p values
  chi_sq_p_value <- vapply(1:nrow(dt02), function(i) {
    dt03 <- dt01[iv %in% dt02[i, ]]
    stats::chisq.test(dt03$iv, dt03$dv, correct = FALSE)[["p.value"]]
  }, FUN.VALUE = numeric(1L))
  # bonferroni
  bonferroni_sig <- ifelse(
    chi_sq_p_value < .05 / nrow(dt02), "Yes", "No")
  # output
  output <- data.table::data.table(
    dt02, section_2, chi_sq_p_value =
      kim::pretty_round_p_value(chi_sq_p_value),
    bonferroni_sig)
  return(output)
}
