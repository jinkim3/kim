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
    dot_size = 5,
    line_thickness = 1.5) {
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
  # stats::na.omit(
  sample_size <- nrow(dt)
  # set default number of iterations
  if (is.null(iterations)) {
    iterations <- 0
  }
  # parallel analysis
  pa_result <- parallel_analysis_function(
    x = dt,
    iterations = iterations, centile = percentile_for_eigenvalue,
    quietly = FALSE,
    status = TRUE, all = TRUE, cfa = F, graph = TRUE, color = TRUE,
    col = c("black", "red", "blue"), lty = c(1, 2, 3),
    lwd = 1, legend = TRUE, file = "", width = 640,
    height = 640, grdevice = "png", seed = 0
  )
  # extract the results
  number_of_retained_factors <- max(pa_result[["Retained"]])
  unadjusted_eigenvalues <- pa_result[["Ev"]]
  eigenvalues_of_random_data <- pa_result[["RndEv"]]
  component_number <- seq_along(dt)
  # create a new dt for plotting
  dt2 <- data.table::data.table(
    component_number,
    "eigenvalue_type" =
      rep(
        "Unadjusted Eigenvalue",
        length(unadjusted_eigenvalues)
      ),
    "eigenvalue" = unadjusted_eigenvalues
  )
  dt3 <- data.table::data.table(
    component_number,
    "eigenvalue_type" =
      rep(
        paste0(
          "Eigenvalues of Random Data (",
          percentile_for_eigenvalue, "th percentile)"
        ),
        length(eigenvalues_of_random_data)
      ),
    "eigenvalue" = eigenvalues_of_random_data
  )
  dt4 <- rbind(dt2, dt3)
  dt5 <- subset(dt4, component_number == 1)
  # plot
  g1 <- ggplot2::ggplot(dt4, ggplot2::aes(
    x = component_number,
    y = eigenvalue,
    group = eigenvalue_type,
    color = eigenvalue_type,
    linetype = eigenvalue_type))
  g1 <- g1 + ggplot2::geom_point(size = dot_size)
  g1 <- g1 + ggplot2::geom_line(size = line_thickness)
  g1 <- g1 + scale_linetype_manual(values = line_types)
  g1 <- g1 + ggplot2::geom_text(data = dt5, ggplot2::aes(
    x = component_number, y = eigenvalue, label = eigenvalue_type,
    color = eigenvalue_type, hjust = -0.1, vjust = -0.1),
    fontface = "bold", size = 4, inherit.aes = FALSE)
  g1 <- g1 + ggplot2::theme_classic(base_size = 16) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "none",
      axis.text = ggplot2::element_text(color = "black", size = 12),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 12)),
      axis.title.y = ggplot2::element_text(
        angle = 90, margin = ggplot2::margin(r = 12)))
  g1 <- g1 + ggplot2::theme(
    plot.subtitle = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_x_continuous(breaks = component_number) +
    ggplot2::xlab("\nComponent Number") +
    ggplot2::ylab("Eigenvalue\n")
  g1 <- g1 + ggplot2::labs(
    title = paste0(
      "Parallel Analysis Result: ", number_of_retained_factors,
      " Factor",
      sprintf("%s", ifelse(number_of_retained_factors > 1, "s", "")),
      " to Retain\n"
    )
  )
  g1 <- g1 + ggplot2::labs(subtitle = bquote(
    italic(N) ~ " = " ~ .(sample_size)
  ))
  return(g1)
}
