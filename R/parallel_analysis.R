#' Parallel analysis
#'
#' Conducts a parallel analysis to determine how many factors
#' to retain in a factor analysis.
#'
#' The following package(s) must be installed prior to running the function:
#' Package 'paran' v1.5.2 (or possibly a higher version) by
#' Alexis Dinno (2018),
#' <https://cran.r-project.org/package=paran>
#'
#' @param data a data object (a data frame or a data.table)
#' @param names_of_vars names of the variables
#' @param iterations number of random data sets. If no input is entered,
#' this value will be set as 30 * number of variables.
#' @param percentile_for_eigenvalue percentile used in estimating bias
#' (default = 95).
#' @param line_types types of the lines connecting eigenvalues.
#' By default, \code{line_types = c("dashed", "solid")}
#' @param colors size of the dots denoting eigenvalues (default = 5).
#' @param eigenvalue_random_label_x_pos (optional) x coordinate of
#' the label for eigenvalues from randomly generated data.
#' @param eigenvalue_random_label_y_pos (optional) y coordinate of
#' the label for eigenvalues from randomly generated data.
#' @param unadj_eigenvalue_label_x_pos (optional) x coordinate of
#' the label for unadjusted eigenvalues
#' @param unadj_eigenvalue_label_y_pos (optional) y coordinate of
#' the label for unadjusted eigenvalues
#' @param label_offset_percent How much should labels for the
#' eigenvalue curves be offset, as a percentage of the plot's
#' x and y range? (default = 2)
#' @param label_size size of the labels for the eigenvalue curves
#' (default = 6).
#' @param dot_size size of the dots denoting eigenvalues (default = 5).
#' @param line_thickness thickness of the eigenvalue curves (default = 1.5).
#' @param y_axis_title_vjust position of the y axis title as a
#' proportion of the range (default = 0.8).
#' @param title_text_size size of the plot title (default = 26).
#' @param axis_text_size size of the text on the axes (default = 22).
#' @examples
#' \donttest{
#' parallel_analysis(
#'   data = mtcars, names_of_vars = c("disp", "hp", "drat"))
#' # parallel_analysis(
#' # data = mtcars, names_of_vars = c("carb", "vs", "gear", "am"))
#' }
#' @export
# parallel analysis factor analysis
parallel_analysis <- function(
    data = NULL,
    names_of_vars = NULL,
    iterations = NULL,
    percentile_for_eigenvalue = 95,
    line_types = c("dashed", "solid"),
    colors = c("red", "blue"),
    eigenvalue_random_label_x_pos = NULL,
    eigenvalue_random_label_y_pos = NULL,
    unadj_eigenvalue_label_x_pos = NULL,
    unadj_eigenvalue_label_y_pos = NULL,
    label_offset_percent = 2,
    label_size = 6,
    dot_size = 5,
    line_thickness = 1.5,
    y_axis_title_vjust = 0.8,
    title_text_size = 26,
    axis_text_size = 22) {
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
  # check if Package 'paran' is installed
  if (!"paran" %in% installed_pkgs) {
    message(paste0(
      "To conduct a parallel analysis, Package 'paran' must ",
      "be installed.\nTo install Package 'paran', type ",
      "'kim::prep(paran)'",
      "\n\nAlternatively, to install all packages (dependencies) required ",
      "for all\nfunctions in Package 'kim', type ",
      "'kim::install_all_dependencies()'"))
    return()
  } else {
    # proceed if Package 'paran' is already installed
    parallel_analysis_function <- utils::getFromNamespace("paran", "paran")
  }
  # check inputs
  if (is.null(data)) {
    stop("Please enter an input for the `data` argument")
  }
  if (is.null(names_of_vars)) {
    stop("Please enter an input for the `names_of_vars` argument")
  }
  # bind the vars locally to the function
  eigenvalue <- eigenvalue_type <- NULL
  # convert to data table and omit na
  dt <- data.table::setDT(data.table::copy(
    data))[, names_of_vars, with = FALSE]
  num_of_rows_original <- nrow(dt)
  # exclude na
  dt <- stats::na.omit(dt)
  num_of_rows_new <- nrow(dt)
  if (num_of_rows_new != num_of_rows_original) {
    num_of_rows_removed <- num_of_rows_original - num_of_rows_new
    if (num_of_rows_removed == num_of_rows_original) {
      message(paste0(
        "Removing rows with missing value(s) resulted in an empty",
        " data set.\nPlease check your data input."))
    } else {
      message(paste0(
        num_of_rows_removed,
        " rows (",
        signif(num_of_rows_removed / num_of_rows_original * 100, 2),
        "%) were removed due to missing values."))
    }
  }
  # set default number of iterations
  if (is.null(iterations)) {
    iterations <- 0
  }
  # parallel analysis
  pa_result <- parallel_analysis_function(
    x = dt,
    iterations = iterations, centile = percentile_for_eigenvalue,
    quietly = FALSE,
    status = TRUE, all = TRUE, cfa = FALSE, graph = TRUE, color = TRUE,
    col = c("black", "red", "blue"), lty = c(1, 2, 3),
    lwd = 1, legend = TRUE, file = "", width = 640,
    height = 640, grdevice = "png", seed = 0)
  # extract the results
  number_of_retained_factors <- max(pa_result[["Retained"]])
  unadjusted_eigenvalues <- pa_result[["Ev"]]
  eigenvalues_of_random_data <- pa_result[["RndEv"]]
  component_number <- seq_along(dt)
  # set default parameters for plotting
  if (is.null(eigenvalue_random_label_x_pos)) {
    eigenvalue_random_label_x_pos <- min(component_number) +
      (max(component_number) - min(component_number)) *
      (label_offset_percent / 100)
  }
  if (is.null(eigenvalue_random_label_y_pos)) {
    eigenvalue_random_label_y_pos <- eigenvalues_of_random_data[1] +
      (max(unadjusted_eigenvalues) - min(unadjusted_eigenvalues)) *
      (label_offset_percent / 100)
  }
  if (is.null(unadj_eigenvalue_label_x_pos)) {
    unadj_eigenvalue_label_x_pos <- min(component_number) +
      (max(component_number) - min(component_number)) *
      (label_offset_percent / 100)
  }
  if (is.null(unadj_eigenvalue_label_y_pos)) {
    unadj_eigenvalue_label_y_pos <- unadjusted_eigenvalues[1] +
      (max(unadjusted_eigenvalues) - min(unadjusted_eigenvalues)) *
      (label_offset_percent / 100)
  }
  # plot each layer
  g1 <- ggplot2::ggplot()
  # unadjusted eigenvalues
  g1 <- g1 + ggplot2::geom_point(aes(
    x = component_number, y = unadjusted_eigenvalues),
    color = colors[2], size = dot_size)
  g1 <- g1 + ggplot2::geom_line(aes(
    x = component_number, y = unadjusted_eigenvalues),
    color = colors[2], size = line_thickness,
    linetype = line_types[2])
  # label
  g1 <- g1 + annotate(
    geom = "text",
    x = unadj_eigenvalue_label_x_pos,
    y = unadj_eigenvalue_label_y_pos,
    label = "Unadjusted Eigenvalues",
    hjust = 0,
    vjust = 0,
    size = label_size,
    color = colors[2],
    fontface = "bold")
  # eigenvalues from randomly generated data
  g1 <- g1 + ggplot2::geom_point(aes(
    x = component_number, y = eigenvalues_of_random_data),
    color = colors[1], size = dot_size)
  g1 <- g1 + ggplot2::geom_line(aes(
    x = component_number, y = eigenvalues_of_random_data),
    color = colors[1], size = line_thickness,
    linetype = line_types[1])
  # label
  g1 <- g1 + annotate(
    geom = "text",
    x = eigenvalue_random_label_x_pos,
    y = eigenvalue_random_label_y_pos,
    label = paste0(
      "Eigenvalues of Random Data (",
      percentile_for_eigenvalue, "th percentile)"),
    hjust = 0,
    vjust = 0,
    size = label_size,
    color = colors[1],
    fontface = "bold")
  # title
  plot_title <- paste0(
    "Parallel Analysis Suggests Retention of ",
    number_of_retained_factors,
    " Factor",
    ifelse(number_of_retained_factors > 1, "s", ""),
    "\n\n(N = ", sample_size, ")")
  g1 <- g1 + ggplot2::labs(
    title = plot_title,
    x = "Component Number",
    y = "Eigenvalue")
  g1 <- g1 + ggplot2::theme_classic(base_size = axis_text_size)
  g1 <- g1 + ggplot2::theme(
    plot.title = ggplot2::element_text(
      color = "black", size = title_text_size, hjust = 0.5,
      face = "bold"),
    legend.position = "none",
    axis.text = ggplot2::element_text(
      color = "black", hjust = 0.5,
      face = "bold"),
    axis.title.x = ggplot2::element_text(
      margin = ggplot2::margin(t = 12),
      color = "black", hjust = 0.5,
      face = "bold"),
    axis.title.y = ggplot2::element_text(
      color = "black", hjust = 0.5,
      face = "bold",
      angle = 0, vjust = y_axis_title_vjust))
  g1 <- g1 + ggplot2::scale_x_continuous(breaks = component_number)
  # g1 <- g1 + ggplot2::labs(subtitle = bquote(
  #   italic(N) ~ " = " ~ .(sample_size)
  # ))
  return(g1)
}
