#' Barplot for counts
#'
#' @param data a data object (a data frame or a data.table)
#' @param x name of the variable that will be on the x axis of the barplot
#' @param y name of the variable that will be on the y axis of the barplot
#' @examples
#' barplot_for_counts(
#'   data = data.frame(
#'     cyl = names(table(mtcars$cyl)),
#'     count = as.vector(table(mtcars$cyl))
#'   ),
#'   x = "cyl", y = "count"
#' )
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
  } else {
    # proceed if Package 'ggplot2' is already installed
    kim::prep("ggplot2", silent_if_successful = TRUE)
  }
  # set data to be a data.table
  data <- data.table::setDT(copy(data))
  # check for x axis
  if (!is.null(data) & x %in% names(data) & y %in% names(data)) {
    dt <- data[, c(x, y), with = FALSE]
    g1 <- ggplot(dt, aes(x = get(x), y = get(y)))
    g1 <- g1 + xlab(x)
    g1 <- g1 + ylab(y)
  }
  # set up data table
  if (is.null(data)) {
    dt <- data.table::data.table(x = x, y = y)
    g1 <- ggplot(dt, aes(x = x, y = y))
  }
  g1 <- g1 + geom_bar(stat = "identity")
  # plot theme
  g1 <- g1 + theme_classic(base_size = 20) %+replace%
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 24)),
      axis.title.y = element_text(
        angle = 0,
        vjust = 0.85,
        margin = margin(r = 24)
      ),
      axis.title = element_text(
        face = "bold",
        color = "black",
        size = 24
      ),
      axis.text = element_text(
        face = "bold",
        color = "black",
        size = 20
      ),
      legend.title = element_text(
        face = "bold",
        color = "black",
        size = 24
      ),
      legend.text = element_text(
        face = "bold",
        color = "black",
        size = 20
      )
    )
  return(g1)
}
