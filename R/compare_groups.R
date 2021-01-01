#' Compare groups
#'
#' Compares groups by (1) creating histogram by group; (2) summarizing
#' descriptive statistics by group; and (3) conducting pairwise
#' comparisons (t-tests and Mann-Whitney tests).
#'
#' @param data a data object (a data frame or a data.table)
#' @param iv_name name of the independent variable (grouping variable)
#' @param dv_name name of the dependent variable (measure variable
#' of interest)
#' @param sigfigs number of significant digits to round to
#' @return the output will be a list of (1) ggplot object
#' (histogram by group) (2) a data.table with descriptive statistics by
#' group; and (3) a data.table with pairwise comparison results
#' @examples
#' compare_groups(data = iris, iv_name = "Species", dv_name = "Sepal.Length")
#' @export
#' @import data.table
compare_groups <- function(
  data = NULL,
  iv_name = NULL,
  dv_name = NULL,
  sigfigs = 3) {
  # histogram by group
  output_1 <- kim::histogram_by_group(
    data = data, iv_name = iv_name, dv_name = dv_name)
  # descriptive stats by group
  output_2 <- kim::desc_stats_by_group(
    data = data, var_for_stats = dv_name, grouping_vars = iv_name,
    sigfigs = sigfigs)
  # pairwise comparison results
  output_3 <- kim::t_test_pairwise(
    data = data, iv_name = iv_name, dv_name = dv_name)
  # print outputs
  output_1
  output_2
  output_3
  # return output
  output_list <- list(output_1, output_2, output_3)
  names(output_list) <- c(
    "histogram", "desc_stats", "pairwise")
  return(output_list)
}