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
#' @param stats statistics to calculate for each group.
#' If \code{stats = "basic"},
#' group size, mean, standard deviation, median, minimum, and maximum will
#' be calculated. If \code{stats = "all"}, in addition to the
#' aforementioned statistics, standard error, 95% confidence and
#' prediction intervals, skewness, and kurtosis will also be calculated.
#' The \code{stats} argument can also be a character vector with types of
#' statistics to calculate. For example, entering
#' \code{stats = c("mean", "median")} will calculate mean and median.
#' By default, \code{stats = "basic"}
#' @param cohen_d if \code{cohen_d = TRUE}, Cohen's d statistics will be
#' included in the pairwise comparison data.table.
#' @param cohen_d_w_ci if \code{cohen_d_w_ci = TRUE},
#' Cohen's d with 95% CI will be included in the output data.table.
#' @param bonferroni if \code{bonferroni = TRUE}, Bonferroni tests will be
#' conducted for t-tests or Mann-Whitney tests.
#' @param mann_whitney if \code{TRUE}, Mann-Whitney test results will be
#' included in the pairwise comparison data.table.
#' If \code{FALSE}, Mann-Whitney tests will not be performed.
#' @param t_test_stats if \code{t_test_stats = TRUE}, t-test statistic
#' and degrees of freedom will be included in the pairwise comparison
#' data.table.
#' @param t_test_df_decimals number of decimals for the degrees of freedom
#' in t-tests (default = 1)
#' @param round_p number of decimal places to which to round
#' p-values (default = 3)
#' @param save_as_png if \code{save = "all"} or if \code{save = TRUE},
#' the histogram by group, descriptive statistics by group,
#' and pairwise comparison results will be saved as a PNG file.
#' @param png_name name of the PNG file to be saved. By default, the name
#' will be "compare_groups_results_" followed by a timestamp of the
#' current time.
#' The timestamp will be in the format, jan_01_2021_1300_10_000001,
#' where "jan_01_2021" would indicate January 01, 2021;
#' 1300 would indicate 13:00 (i.e., 1 PM); and 10_000001 would
#' indicate 10.000001 seconds after the hour.
#' @param xlab title of the x-axis for the histogram by group.
#' If \code{xlab = FALSE}, the title will be removed. By default
#' (i.e., if no input is given), \code{dv_name} will be used as
#' the title.
#' @param ylab title of the y-axis for the histogram by group.
#' If \code{ylab = FALSE}, the title will be removed. By default
#' (i.e., if no input is given), \code{iv_name} will be used as
#' the title.
#' @param x_limits a numeric vector with values of the endpoints
#' of the x axis.
#' @param x_breaks a numeric vector indicating the points at which to
#' place tick marks on the x axis.
#' @param x_labels a vector containing labels for the place tick marks
#' on the x axis.
#' @param width width of the PNG file (default = 4000)
#' @param height height of the PNG file (default = 3000)
#' @param units the units for the \code{width} and \code{height} arguments.
#' Can be \code{"px"} (pixels), \code{"in"} (inches), \code{"cm"},
#' or \code{"mm"}. By default, \code{units = "px"}.
#' @param res The nominal resolution in ppi which will be recorded
#' in the png file, if a positive integer. Used for units
#' other than the default. If not specified, taken as 300 ppi
#' to set the size of text and line widths.
#' @param layout_matrix The layout argument for arranging plots and tables
#' using the \code{grid.arrange} function.
#' @param col_names_nicer if \code{col_names_nicer = TRUE}, column names
#' will be converted from snake_case to an easier-to-eye format.
#' @return the output will be a list of (1) ggplot object
#' (histogram by group) (2) a data.table with descriptive statistics by
#' group; and (3) a data.table with pairwise comparison results.
#' If \code{save_as_png = TRUE}, the plot and tables will be also saved
#' on local drive as a PNG file.
#' @examples
#' \dontrun{
#' compare_groups(data = iris, iv_name = "Species", dv_name = "Sepal.Length")
#' compare_groups(data = iris, iv_name = "Species", dv_name = "Sepal.Length",
#' x_breaks = 4:8)
#' }
#' @export
#' @import data.table
compare_groups <- function(
  data = NULL,
  iv_name = NULL,
  dv_name = NULL,
  sigfigs = 3,
  stats = "basic",
  cohen_d = TRUE,
  cohen_d_w_ci = TRUE,
  bonferroni = FALSE,
  mann_whitney = TRUE,
  t_test_stats = FALSE,
  t_test_df_decimals = 1,
  round_p = 3,
  save_as_png = FALSE,
  png_name = NULL,
  xlab = NULL,
  ylab = NULL,
  x_limits = NULL,
  x_breaks = NULL,
  x_labels = NULL,
  width = 4000,
  height = 3000,
  units = "px",
  res = 300,
  layout_matrix = NULL,
  col_names_nicer = TRUE) {
  # histogram by group
  output_1 <- kim::histogram_by_group(
    data = data, iv_name = iv_name, dv_name = dv_name,
    xlab = xlab, ylab = ylab,
    x_limits = x_limits,
    x_breaks = x_breaks,
    x_labels = x_labels,
    sigfigs = sigfigs)
  # descriptive stats by group
  output_2 <- kim::desc_stats_by_group(
    data = data, var_for_stats = dv_name, grouping_vars = iv_name,
    sigfigs = sigfigs, stats = stats)
  # pairwise comparison results
  output_3 <- kim::t_test_pairwise(
    data = data, iv_name = iv_name, dv_name = dv_name,
    sigfigs = sigfigs,
    cohen_d = cohen_d,
    cohen_d_w_ci = cohen_d_w_ci,
    bonferroni = bonferroni,
    mann_whitney = mann_whitney,
    t_test_stats = t_test_stats,
    t_test_df_decimals = t_test_df_decimals,
    round_p = round_p)
  # print outputs
  output_1
  output_2
  output_3
  # nicer column names
  if (col_names_nicer == TRUE) {
    # descriptive stats
    names(output_2) <- gsub("^n$", "N", names(output_2))
    names(output_2) <- gsub("^mean$", "Mean", names(output_2))
    names(output_2) <- gsub("^sd$", "SD", names(output_2))
    names(output_2) <- gsub("^median$", "Median", names(output_2))
    names(output_2) <- gsub("^min$", "Min", names(output_2))
    names(output_2) <- gsub("^max$", "Max", names(output_2))
    # pairwise comparisons
    names(output_3) <- gsub("^group_1$", "Group 1", names(output_3))
    names(output_3) <- gsub("^group_2$", "Group 2", names(output_3))
    names(output_3) <- gsub("^group_1_n$", "Group 1 N", names(output_3))
    names(output_3) <- gsub("^group_2_n$", "Group 2 N", names(output_3))
    names(output_3) <- gsub(
      "^group_1_mean$", "Group 1 Mean", names(output_3))
    names(output_3) <- gsub(
      "^group_2_mean$", "Group 2 Mean", names(output_3))
    names(output_3) <- gsub("^cohen_d$", "Cohen's d", names(output_3))
    names(output_3) <- gsub(
      "^cohen_d_w_95_ci$", "Cohen's d and 95% CI", names(output_3))
    names(output_3) <- gsub(
      "^t_test_p_value$", "t-test p", names(output_3))
    names(output_3) <- gsub(
      "^mann_whitney_p_value$", "Mann-Whitney p", names(output_3))
  }
  # save as png
  if (save_as_png == "all" | save_as_png == TRUE | !is.null(png_name)) {
    # installed packages
    installed_pkgs <- rownames(utils::installed.packages())
    # required packages
    # required_pkgs <-
    # check if Package 'gridExtra' is installed
    if (!"gridExtra" %in% installed_pkgs) {
      message(paste0(
        "This function requires the installation of Package 'gridExtra'.",
        "\nTo install Package 'gridExtra', type ",
        "'kim::prep(gridExtra)'",
        "\n\nAlternatively, to install all packages (dependencies) required ",
        "for all\nfunctions in Package 'kim', type ",
        "'kim::install_all_dependencies()'"))
      return()
    } else {
      # proceed if Package 'gridExtra' is already installed
      table_grob_from_grid_extra <- utils::getFromNamespace(
        "tableGrob", "gridExtra")
      grid_arrange_from_grid_extra <- utils::getFromNamespace(
        "grid.arrange", "gridExtra")
    }
    # default file name
    if (is.null(png_name)) {
      ts <- tolower(
        gsub("\\.", "_", format(Sys.time(), "_%b_%d_%Y_%H%M_%OS6")))
      png_name <- paste0("compare_results", ts)
    }
    # initialize the png
    grDevices::png(paste0(png_name, ".png"),
                   height = height, width = width, units = units, res = res)
    # grobs
    grob_1 <- output_1
    grob_2 <- grid::textGrob("Descriptive Statistics: ")
    grob_3 <- table_grob_from_grid_extra(output_2)
    grob_4 <- grid::textGrob("Pairwise Comparisons: ")
    grob_5 <- table_grob_from_grid_extra(output_3)
    # grob list
    grob_list <- list(grob_1, grob_2, grob_3, grob_4, grob_5)
    # layout matrix
    if (is.null(layout_matrix)) {
      # get number of groups
      number_of_groups <- nrow(output_2)
      if (number_of_groups == 2) {
        layout_matrix <- rbind(
          c(1,1,1,1,1,1,1),
          c(1,1,1,1,1,1,1),
          c(1,1,1,1,1,1,1),
          c(1,1,1,1,1,1,1),
          c(1,1,1,1,1,1,1),
          c(2,3,3,3,3,3,3),
          c(4,5,5,5,5,5,5))
      } else if (number_of_groups == 3) {
        layout_matrix <- rbind(
          c(1,1,1,1,1,1,1),
          c(1,1,1,1,1,1,1),
          c(1,1,1,1,1,1,1),
          c(1,1,1,1,1,1,1),
          c(1,1,1,1,1,1,1),
          c(2,3,3,3,3,3,3),
          c(4,5,5,5,5,5,5))
      } else if (number_of_groups == 4) {
        layout_matrix <- rbind(
          c(1,1,1,1,1,1,1),
          c(1,1,1,1,1,1,1),
          c(1,1,1,1,1,1,1),
          c(1,1,1,1,1,1,1),
          c(1,1,1,1,1,1,1),
          c(1,1,1,1,1,1,1),
          c(1,1,1,1,1,1,1),
          c(2,3,3,3,3,3,3),
          c(2,3,3,3,3,3,3),
          c(4,5,5,5,5,5,5),
          c(4,5,5,5,5,5,5))
      } else if (number_of_groups >= 5) {
        layout_matrix <- rbind(
          c(1,1,1,1,1,1,1),
          c(1,1,1,1,1,1,1),
          c(1,1,1,1,1,1,1),
          c(1,1,1,1,1,1,1),
          c(1,1,1,1,1,1,1),
          c(1,1,1,1,1,1,1),
          c(1,1,1,1,1,1,1),
          c(1,1,1,1,1,1,1),
          c(1,1,1,1,1,1,1),
          c(1,1,1,1,1,1,1),
          c(2,3,3,3,3,3,3),
          c(2,3,3,3,3,3,3),
          c(2,3,3,3,3,3,3),
          c(2,3,3,3,3,3,3),
          c(4,5,5,5,5,5,5),
          c(4,5,5,5,5,5,5),
          c(4,5,5,5,5,5,5),
          c(4,5,5,5,5,5,5),
          c(4,5,5,5,5,5,5),
          c(4,5,5,5,5,5,5),
          c(4,5,5,5,5,5,5))
      }
    }
    grid_arrange_from_grid_extra(
      grobs = grob_list, layout_matrix = layout_matrix)
    grDevices::dev.off()
  }
  # return output
  output_list <- list(output_1, output_2, output_3)
  names(output_list) <- c(
    "histogram", "desc_stats", "pairwise")
  return(output_list)
}
