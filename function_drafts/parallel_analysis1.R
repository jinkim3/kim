#' Parallel analysis
#'
#' Conducts a parallel analysis to determine how many factors
#' to retain in a factor analysis.
#' Uses the 'paran' package v1.5.2 by
#' Dinno (2018) <https://cran.r-project.org/web/packages/paran/index.html>
#'
#' @param data a data object (a data frame or a data.table)
#' @param names_of_vars names of the variables
#' @param iterations number of random data sets. If no input is entered,
#' this value will be set as 30 * number of variables.
#' @param percentile_for_eigenvalue percentile used in estimating bias
#' (default = 95).
#'
#' @examples
#' parallel_analysis(
#'   data = mtcars, names_of_vars = c("disp", "hp", "drat")
#' )
#' @export
#' @import paran
# parallel analysis factor analysis
parallel_analysis <- function(
                              data = NULL,
                              names_of_vars = NULL,
                              iterations = NULL,
                              percentile_for_eigenvalue = 95) {
  # omit na
  df <- na.omit(data[names_of_vars])
  sample_size <- nrow(df)
  # set default number of iterations
  if (is.null(iterations)) {
    iterations <- 0
  }
  # parallel analysis
  pa_result <- paran::paran(
    x = df,
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
  component_number <- seq_along(df)
  # create a new df for plotting
  df2 <- data.frame(
    component_number,
    eigenvalue_type =
      rep(
        "Unadjusted Eigenvalue",
        length(unadjusted_eigenvalues)
      ),
    eigenvalue = unadjusted_eigenvalues
  )
  df3 <- data.frame(
    component_number,
    eigenvalue_type =
      rep(
        paste0(
          "Eigenvalues of Random Data (",
          percentile_for_eigenvalue, "th percentile)"
        ),
        length(eigenvalues_of_random_data)
      ),
    eigenvalue = eigenvalues_of_random_data
  )
  df4 <- rbind(df2, df3)
  df5 <- subset(df4, component_number == 1)
  # plot
  g1 <- ggplot(df4, aes(
    x = component_number,
    y = eigenvalue,
    group = eigenvalue_type
  )) +
    geom_point(aes(color = eigenvalue_type),
      size = 4
    ) +
    geom_line(aes(color = eigenvalue_type),
      size = 2
    ) +
    geom_text(
      data = df5,
      aes(
        x = component_number,
        y = eigenvalue,
        label = eigenvalue_type,
        color = eigenvalue_type,
        hjust = -0.1, vjust = -0.1
      ),
      fontface = "bold",
      size = 4,
      inherit.aes = F
    ) +
    theme_classic(base_size = 16) %+replace%
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "none",
      axis.text = element_text(color = "black", size = 12),
      axis.title.x = element_text(margin = margin(t = 12)),
      axis.title.y = element_text(
        angle = 90, margin = margin(r = 12)
      )
    ) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = component_number) +
    xlab("\nComponent Number") +
    ylab("Eigenvalue\n")
  # add titles
  subtitle <- expression(
    paste0("italic(N)", " = ", ")\n")
  )
  g1 <- g1 + labs(
    title = paste0(
      "Parallel Analysis Result: ",
      number_of_retained_factors,
      " Factor(s) to Retain\n"
    ),
    subtitle = subtitle
  )
  return(g1)
}
parallel_analysis(
  data = mtcars, names_of_vars = c("disp", "hp", "drat")
)
prep(paran)
