#' Barplot for counts
#'
#' @param data a data object (a data frame or a data.table)
#' @param x name of the variable that will be on the x axis of the barplot
#' @param y name of the variable that will be on the y axis of the barplot
#' @examples
#' \donttest{
#' barplot_for_counts(x = 1:3, y = 7:9)
#' barplot_for_counts(data = data.frame(
#' cyl = names(table(mtcars$cyl)), count = as.vector(table(mtcars$cyl))),
#' x = "cyl", y = "count")
#' }
#' @export
barplot_for_counts <- function(data = NULL, x, y) {
  # check if Package 'ggplot2' is installed
  if (!"ggplot2" %in% rownames(utils::installed.packages())) {
    message(paste0(
      "This function requires the installation of Package 'ggplot2'.",
      "\nTo install Package 'ggplot2', type ",
      "'kim::prep(ggplot2)'",
      "\n\nAlternatively, to install all packages (dependencies) required ",
      "for all\nfunctions in Package 'kim', type ",
      "'kim::install_all_dependencies()'"))
    return()
  }
  # set up data table
  if (is.null(data)) {
    dt <- data.table::data.table(x = x, y = y)
    g1 <- ggplot2::ggplot(dt, ggplot2::aes(x = x, y = y))
  } else {
    # set data to be a data.table
    data <- data.table::setDT(data.table::copy(data))
    if (!x %in% names(data)) {
      stop(paste0('The column "', x, '" is not in the data.'))
    }
    if (!y %in% names(data)) {
      stop(paste0('The column "', y, '" is not in the data.'))
    }
    dt <- data[, c(x, y), with = FALSE]
    g1 <- ggplot2::ggplot(dt, ggplot2::aes(x = get(x), y = get(y)))
    g1 <- g1 + ggplot2::xlab(x)
    g1 <- g1 + ggplot2::ylab(y)
  }
  g1 <- g1 + ggplot2::geom_bar(stat = "identity")
  # plot theme
  g1 <- g1 + ggplot2::theme_classic(base_size = 20) + ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5),
    axis.title.x = ggplot2::element_text(
      margin = ggplot2::margin(t = 24)),
    axis.title.y = ggplot2::element_text(
      angle = 0,
      vjust = 0.85,
      margin = ggplot2::margin(r = 24)),
    axis.title = ggplot2::element_text(
      face = "bold",
      color = "black",
      size = 24),
    axis.text = ggplot2::element_text(
      face = "bold",
      color = "black",
      size = 20),
    legend.title = ggplot2::element_text(
      face = "bold",
      color = "black",
      size = 24),
    legend.text = ggplot2::element_text(
      face = "bold",
      color = "black",
      size = 20))
  return(g1)
}
