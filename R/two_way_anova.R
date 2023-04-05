#' Two-way ANOVA
#'
#' Conduct a two-way analysis of variance (ANOVA).
#'
#' The following package(s) must be installed prior to running this function:
#' Package 'car' v3.0.9 (or possibly a higher version) by
#' Fox et al. (2020),
#' <https://cran.r-project.org/package=car>
#'
#' If robust ANOVA is to be conducted, the following package(s)
#' must be installed prior to running the function:
#' Package 'WRS2' v1.1-1 (or possibly a higher version) by
#' Mair & Wilcox (2021),
#' <https://cran.r-project.org/package=WRS2>
#'
#' @param data a data object (a data frame or a data.table)
#' @param dv_name name of the dependent variable
#' @param iv_1_name name of the first independent variable
#' @param iv_2_name name of the second independent variable
#' @param iv_1_values restrict all analyses to observations having
#' these values for the first independent variable
#' @param iv_2_values restrict all analyses to observations having
#' these values for the second independent variable
#' @param sigfigs number of significant digits to which to round
#' values in anova table (default = 3)
#' @param robust if \code{TRUE}, conduct a robust ANOVA in addition.
#' @param iterations number of bootstrap samples for robust ANOVA.
#' The default is set at 2000, but consider increasing the number
#' of samples to 5000, 10000, or an even larger number, if slower
#' handling time is not an issue.
#' @param plot if \code{TRUE}, print a plot and enable returning an output.
#' @param error_bar if \code{error_bar = "se"}; error bars will be +/-1
#' standard error; if \code{error_bar = "ci"} error bars will be a
#' confidence interval
#' @param error_bar_range width of the confidence interval
#' (default = 0.95 for 95 percent confidence interval).
#' This argument will not apply when \code{error_bar = "se"}
#' @param line_size thickness of the lines connecting group means,
#' (default = 1)
#' @param dot_size size of the dots indicating group means (default = 3)
#' @param error_bar_tip_width graphically, width of the segments
#' at the end of error bars (default = 0.13)
#' @param position_dodge by how much should the group means and error bars
#' be horizontally offset from each other so as not to overlap?
#' (default = 0.13)
#' @param legend_position position of the legend:
#' \code{"none", "top", "right", "bottom", "left", "none"}
#' (default = \code{"right"})
#' @param output output type can be one of the following: \code{"anova_table"},
#' \code{"group_stats"}, \code{"plot"},
#' \code{"robust_anova_results"}, \code{"robust_anova_post_hoc_results"},
#' \code{"robust_anova_post_hoc_contrast"}, \code{"all"}
#' @param png_name name of the PNG file to be saved.
#' If \code{png_name = TRUE}, the name will be "two_way_anova_"
#' followed by a timestamp of the current time.
#' The timestamp will be in the format, jan_01_2021_1300_10_000001,
#' where "jan_01_2021" would indicate January 01, 2021;
#' 1300 would indicate 13:00 (i.e., 1 PM); and 10_000001 would
#' indicate 10.000001 seconds after the hour.
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
#' @return by default, the output will be \code{"anova_table"}
#' @examples
#' \donttest{
#' two_way_anova(
#'   data = mtcars, dv_name = "mpg", iv_1_name = "vs",
#'   iv_2_name = "am", iterations = 100)
#' anova_results <- two_way_anova(
#'   data = mtcars, dv_name = "mpg", iv_1_name = "vs",
#'   iv_2_name = "am", output = "all")
#' anova_results
#' }
#' @export
#' @import data.table
two_way_anova <- function(
  data = NULL,
  dv_name = NULL,
  iv_1_name = NULL,
  iv_2_name = NULL,
  iv_1_values = NULL,
  iv_2_values = NULL,
  sigfigs = 3,
  robust = FALSE,
  iterations = 2000,
  plot = FALSE,
  error_bar = "ci",
  error_bar_range = 0.95,
  line_size = 1,
  dot_size = 3,
  error_bar_tip_width = 0.13,
  position_dodge = 0.13,
  legend_position = "right",
  output = "anova_table",
  png_name = NULL,
  width = 5000,
  height = 3600,
  units = "px",
  res = 300,
  layout_matrix = NULL) {
  # installed packages
  installed_pkgs <- rownames(utils::installed.packages())
  # check if Package 'ggplot2' is installed
  if (!"ggplot2" %in% installed_pkgs) {
    message(paste0(
      "This function requires the installation of Package 'ggplot2'.",
      "\nTo install Package 'ggplot2', type ",
      "'kim::prep(ggplot2)'",
      "\n\nAlternatively, to install all packages (dependencies) required ",
      "for all\nfunctions in Package 'kim', type ",
      "'kim::install_all_dependencies()'"))
    return()
  }
  # check if Package 'car' is installed
  if (!"car" %in% installed_pkgs) {
    message(paste0(
      "To conduct a two-way ANOVA, Package 'car' must ",
      "be installed.\nTo install Package 'car', type ",
      "'kim::prep(car)'",
      "\n\nAlternatively, to install all packages (dependencies) required ",
      "for all\nfunctions in Package 'kim', type ",
      "'kim::install_all_dependencies()'"))
    return()
  } else {
    # proceed if Package 'car' is already installed
    levene_test_fn_from_car <- utils::getFromNamespace(
      "leveneTest", "car")
    anova_fn_from_car <- utils::getFromNamespace(
      "Anova", "car")
  }
  # If robust == TRUE, check whether Package 'WRS2' is installed
  if (robust == TRUE) {
    # check if Package 'WRS2' is installed
    if (!"WRS2" %in% installed_pkgs) {
      message(paste0(
        "To conduct floodlight analysis, Package 'WRS2' must ",
        "be installed.\nTo install Package 'WRS2', type ",
        "'kim::prep(WRS2)'",
        "\n\nAlternatively, to install all packages (dependencies) required ",
        "for all\nfunctions in Package 'kim', type ",
        "'kim::install_all_dependencies()'"))
      return()
    } else {
      # proceed if Package 'WRS2' is already installed
      pbad2way_fn_from_wrs2 <- utils::getFromNamespace(
        "pbad2way", "WRS2")
      mcp2a_fn_from_wrs2 <- utils::getFromNamespace(
        "mcp2a", "WRS2")
    }
  }
  # bind the vars locally to the function
  p <- NULL
  # convert data to data table
  dt1 <- data.table::setDT(data.table::copy(data))
  dt1 <- dt1[, c(iv_1_name, iv_2_name, dv_name), with = FALSE]
  # convert iv to factors
  for (col in c(iv_1_name, iv_2_name)) {
    data.table::set(dt1, j = col, value = as.factor(dt1[[col]]))
  }
  # remove na
  dt2 <- stats::na.omit(dt1)
  # missing value message
  missing_value_message <- paste0(
    nrow(dt1) - nrow(dt2),
    " rows were removed due to missing values.")
  # count na rows
  message(missing_value_message)
  # subset for certain values
  if (!is.null(iv_1_values)) {
    dt2 <- dt2[get(iv_1_name) %in% iv_1_values]
  }
  if (!is.null(iv_2_values)) {
    dt2 <- dt2[get(iv_2_name) %in% iv_2_values]
  }
  # stats by iv
  group_stats <- dt2[, list(
    n = .N,
    mean = kim::round_flexibly(mean(get(dv_name)), sigfigs),
    median = kim::round_flexibly(
      as.numeric(stats::median(get(dv_name))), sigfigs),
    sd = kim::round_flexibly(stats::sd(get(dv_name)), sigfigs),
    se = kim::round_flexibly(kim::se_of_mean(get(dv_name)), sigfigs),
    min = kim::round_flexibly(min(get(dv_name)), sigfigs),
    max = kim::round_flexibly(max(get(dv_name)), sigfigs)
  ), keyby = c(iv_1_name, iv_2_name)]
  if (output == "group_stats") {
    return(group_stats)
  }
  message(paste0("\nGroup Statistics on ", dv_name, ":"))
  print(group_stats)
  # print or return plot
  if (plot == TRUE | output %in% c("plot", "all") | !is.null(png_name)) {
    g1 <- kim::plot_group_means(
      data = dt2,
      dv_name = dv_name,
      iv_name = c(iv_1_name, iv_2_name),
      error_bar = error_bar,
      error_bar_range = error_bar_range,
      line_size = line_size,
      dot_size = dot_size,
      error_bar_tip_width = error_bar_tip_width,
      position_dodge = position_dodge,
      legend_position = legend_position)
    if (output == "plot") {
      return(g1)
    }
    print(g1)
  }
  # order the data table
  data.table::setorderv(dt2, c(iv_1_name, iv_2_name))
  # levene's test
  formula_1 <- stats::as.formula(
    paste0(dv_name, " ~ ", iv_1_name, " * ", iv_2_name))
  levene_test_result <- levene_test_fn_from_car(
    y = formula_1, data = dt2)
  levene_test_p_value <- levene_test_result[["Pr(>F)"]][1]
  message("\nLevene's Test Results:\n")
  # update the levene test table
  data.table::setDT(levene_test_result)
  print(levene_test_result)
  if (levene_test_p_value < .05) {
    message(
      "The homogeneity of variance assumption is violated.")
  }
  if (output == "levene_test_result") {
    stop(paste0(
      "The output option of 'levene_test_result' is deprecated as of ",
      "Apr 5, 2023.",
      "\nTo conduct Levene's test, use the function 'levene_test'",
      " instead.\n",
      "Type '?kim::levene_test' for more information."))
  }
  # anova instead of regression
  model_1 <- stats::aov(formula = formula_1, data = dt2)
  anova_table <- anova_fn_from_car(model_1, type = 3)
  source <- row.names(anova_table)
  data.table::setDT(anova_table)
  anova_table <- data.table::data.table(source, anova_table)
  names(anova_table) <- c("source", "type_3_sum_sq", "df", "f", "p")
  # round
  cols_to_round <- c("type_3_sum_sq", "f")
  for (j in cols_to_round) {
    data.table::set(
      anova_table, j = j, value = kim::round_flexibly(
        anova_table[[j]], sigfigs))
  }
  anova_table[, p := kim::pretty_round_p_value(p)]
  message("\nANOVA Results:")
  print(anova_table)
  # save as png
  if (!is.null(png_name)) {
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
        "\n\nAlternatively, to install all packages (dependencies) ",
        "required for all\nfunctions in Package 'kim', type ",
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
    if (png_name == TRUE) {
      # ts stands for time stamp
      ts <- tolower(
        gsub("\\.", "_", format(Sys.time(), "_%b_%d_%Y_%H%M_%OS6")))
      png_name <- paste0("two_way_anova_results", ts)
    }
    # initialize the png
    grDevices::png(
      paste0(png_name, ".png"),
      height = height, width = width, units = units, res = res)
    # grobs
    grob_1 <- grid::textGrob(missing_value_message)
    grob_2 <- grid::textGrob(paste0(
      "\nGroup Statistics on ", dv_name, ": "))
    grob_3 <- table_grob_from_grid_extra(group_stats)
    grob_4 <- grid::textGrob("\nLevene's Test Results: ")
    # levene test
    levene_test_result_text <- kim::levene_test(
      data = dt2,
      dv_name = dv_name,
      iv_1_name = iv_1_name,
      iv_2_name = iv_2_name,
      output_type = "text")
    grob_5 <- grid::textGrob(levene_test_result_text)
    grob_6 <- grid::textGrob("\nANOVA Results: ")
    grob_7 <- table_grob_from_grid_extra(anova_table)
    grob_8 <- grid::textGrob("Plot of Group Means")
    grob_9 <- g1
    # grob list
    grob_list <- list(
      grob_1, grob_2, grob_3, grob_4, grob_5,
      grob_6, grob_7, grob_8, grob_9)
    # layout matrix
    ncol_left <- 17
    ncol_right <- 10
    if (is.null(layout_matrix)) {
      layout_matrix <- rbind(
        c(rep(1, ncol_left), rep(NA, ncol_right)),
        c(rep(2, ncol_left), rep(NA, ncol_right)),
        c(rep(3, ncol_left), rep(NA, ncol_right)),
        c(rep(3, ncol_left), rep(NA, ncol_right)),
        c(rep(3, ncol_left), rep(8, ncol_right)),
        c(rep(3, ncol_left), rep(9, ncol_right)),
        c(rep(4, ncol_left), rep(9, ncol_right)),
        c(rep(5, ncol_left), rep(9, ncol_right)),
        c(rep(5, ncol_left), rep(9, ncol_right)),
        c(rep(6, ncol_left), rep(9, ncol_right)),
        c(rep(7, ncol_left), rep(9, ncol_right)),
        c(rep(7, ncol_left), rep(NA, ncol_right)),
        c(rep(7, ncol_left), rep(NA, ncol_right)),
        c(rep(7, ncol_left), rep(NA, ncol_right)),
        c(rep(7, ncol_left), rep(NA, ncol_right)))
    }
    grid_arrange_from_grid_extra(
      grobs = grob_list, layout_matrix = layout_matrix)
    grDevices::dev.off()
  }
  # output by type
  if (output == "anova_table") {
    invisible(anova_table)
  }
  if (robust == TRUE) {
    # robust anova
    robust_anova_results <- pbad2way_fn_from_wrs2(
      formula_1, data = dt2, est = "mom", nboot = iterations)
    if (output == "robust_anova_results") {
      return(robust_anova_results)
    }
    message("\nRobust ANOVA Results:")
    print(robust_anova_results)
    robust_anova_post_hoc_results <- mcp2a_fn_from_wrs2(
      formula_1, data = dt2, est = "mom", nboot = iterations)
    message("\nRobust ANOVA Post Hoc Test Results:")
    print(robust_anova_post_hoc_results)
    # contrasts
    robust_anova_post_hoc_contrast <-
      robust_anova_post_hoc_results[["contrasts"]]
    message("\nRobust ANOVA Post Hoc Test Contrasts:")
    print(robust_anova_post_hoc_contrast)
  }
  # output for robust anova
  if (output == "robust_anova_post_hoc_results") {
    invisible(robust_anova_post_hoc_results)
  }
  if (output == "robust_anova_post_hoc_contrast") {
    invisible(robust_anova_post_hoc_contrast)
  }
  # return all
  if (output == "all") {
    output <- list(
      "group_stats" = group_stats,
      "levene_test_result" = levene_test_result,
      "anova_table" = anova_table,
      "plot" = g1)
    invisible(output)
  }
}
