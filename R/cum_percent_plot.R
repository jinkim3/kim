#' Cumulative percentage plot
#'
#' Plots or tabulates cumulative percentages associated with
#' elements in a vector
#'
#' @param vector a numeric vector
#' @param output_type if \code{output_type = "plot"}, return a cumulative
#' percentage plot; if \code{output_type = "dt"}, return a data.table
#' with cumulative percentages. By default, \code{output_type = "plot"}
#' @examples
#' \donttest{
#' cum_percent_plot(c(1:100, NA, NA))
#' cum_percent_plot(mtcars$mpg)
#' cum_percent_plot(vector= mtcars$mpg, output_type = "dt")
#' }
#' @export
cum_percent_plot <- function(vector, output_type = "plot") {
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
  g1 <- ggplot2::ggplot(
    data = dt, ggplot2::aes(x = dt$value, y = dt$cum_percent))
  g1 <- g1 + ggplot2::theme_classic(base_size = 16) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text = ggplot2::element_text(
        face = "bold", color = "black", size = 12, hjust = 0.5),
      axis.text.y = ggplot2::element_text(hjust = 0.5),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 12)),
      axis.title.y = ggplot2::element_text(
        vjust = 0.95, margin = ggplot2::margin(r = 12)),
      legend.position = "none")
  g1 <- g1 + ggplot2::geom_line() + ggplot2::geom_point()
  g1 <- g1 + ggplot2::scale_y_continuous(
    breaks = seq(0, 100, 20))
  g1 <- g1 + ggplot2::xlab("Value")
  g1 <- g1 + ggplot2::ylab("Cumulative\nPercentage")
  return(g1)
}
