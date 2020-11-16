#' Barplot for counts
#'
#' @param data a data.frame or data.table object
#' @param x name of the variable that will be on the x axis of the barplot
#' @param y name of the variable that will be on the x axis of the barplot
#' @examples
#' barplot_for_counts(
#' data = data.frame(cyl = names(table(mtcars$cyl)),
#' count = as.vector(table(mtcars$cyl))),
#' x = "cyl", y = "count")
#' @export
#' @import ggplot2
barplot_for_counts <- function(data = NULL, x, y) {
  data <- data.table::setDT(data)
  # check for x axis
  if(!is.null(data) & x %in% names(data) & y %in% names(data)) {
    dt <- data[, c(x, y), with = F]
    p1 <- ggplot(dt, aes(x = get(x), y = get(y)))
    p1 <- p1 + xlab(x)
    p1 <- p1 + ylab(y)
  }
  # set up data table
  if(is.null(data)) {
    dt <- data.table::data.table(x = x, y = y)
    p1 <- ggplot(dt, aes(x = x, y = y))
  }
  p1 <- p1 + geom_bar(stat = "identity")
  return(p1)
}
