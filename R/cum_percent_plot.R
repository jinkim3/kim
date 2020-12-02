#' Cumulative percentage plot
#'
#' Plots or tabulates cumulative percentages associated with
#' elements in a vector
#'
#' @param vector a numeric vector
#' @param output_type if \code{output_type = "plot"}, return a cumulative
#' percentage plot; if \code{output_type = "dt"}, return a data.table
#' with cumulative percentages.
#' @examples
#' cum_percent_plot(c(1:100, NA, NA))
#' cum_percent_plot(mtcars$mpg)
#' cum_percent_plot(vector= mtcars$mpg, output_type = "dt")
#' @export
#' @import ggplot2
cum_percent_plot <- function(vector, output_type = "plot") {
  # omit na
  v_no_na <- stats::na.omit(vector)
  freq_table <- kim::tabulate_vector(
    v_no_na, total_included = FALSE,
    sort_by_increasing_value = TRUE)
  cum_count <- cumsum(freq_table$count)
  cum_percent <- cum_count / length(v_no_na) * 100
  dt <- data.table::data.table(
    value = freq_table$value, cum_count, cum_percent)
  if (output_type == "dt") {
    output <- dt[, cum_percent := cum_percent * 100][]
    return(output)
  }
  # plot
  g1 <- ggplot(data = dt, aes(x = dt$value, y = dt$cum_percent))
  g1 <- g1 + theme_classic(base_size = 16) %+replace%
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text = element_text(
        face = "bold", color = "black", size = 12, hjust = 0.5),
      axis.text.y = element_text(hjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 12)),
      axis.title.y = element_text(vjust = 0.95, margin = margin(r = 12)),
      legend.position = "none")
  g1 <- g1 + geom_line() + geom_point()
  g1 <- g1 + scale_y_continuous(
    breaks = seq(0, 100, 20))
  g1 <- g1 + xlab("Value")
  g1 <- g1 + ylab("Cumulative\nPercentage")
  print(g1)
  return(g1)
}
