#' Scatterplot
#'
#' Creates a scatter plot and calculates a correlation between two variables
#'
#' @param data a data object (a data frame or a data.table)
#' @param x_var_name name of the variable that will go on the x axis
#' @param y_var_name name of the variable that will go on the y axis
#' @param point_label_var_name name of the variable that will be used to
#' label individual observations
#' @param weight_var_name name of the variable by which to weight
#' the individual observations for calculating correlation and plotting
#' the line of fit
#' @param alpha opacity of the dots (0 = completely transparent,
#' 1 = completely opaque)
#' @param annotate_stats if \code{TRUE}, the correlation and p-value will
#' be annotated at the top of the plot
#' @param annotate_y_pos position of the annotated stats, expressed
#' as a percentage of the range of y values by which the annotated
#' stats will be placed above the maximum value of y in the data set
#' (default = 5). If \code{annotate_y_pos = 5}, and the minimum and
#' maximum y values in the data set are 0 and 100, respectively,
#' the annotated stats will be placed at 5% of the y range (100 - 0)
#' above the maximum y value, y = 0.05 * (100 - 0) + 100 = 105.
#' @param line_of_fit_type if \code{line_of_fit_type = "lm"}, a regression
#' line will be fit; if \code{line_of_fit_type = "loess"}, a local
#' regression line will be fit; if \code{line_of_fit_type = "none"},
#' no line will be fit
#' @param ci_for_line_of_fit if \code{ci_for_line_of_fit = TRUE},
#' confidence interval for the line of fit will be shaded
#' @param x_axis_label alternative label for the x axis
#' @param y_axis_label alternative label for the y axis
#' @param point_label_size size for dots' labels on the plot. If no
#' input is entered for this argument, it will be set as
#' \code{point_label_size = 5} by default. If the plot is to be
#' weighted by some variable, this argument will be ignored, and
#' dot sizes will be determined by the argument \code{point_size_range}
#' @param point_size_range minimum and maximum size for dots
#' on the plot when they are weighted
#' @param jitter_x_percent horizontally jitter dots by a percentage of the
#' range of x values
#' @param jitter_y_percent vertically jitter dots by a percentage of the
#' range of y values
#' @param cap_axis_lines logical. Should the axis lines be capped at the
#' outer tick marks? (default = TRUE)
#' @return a ggplot object
#' @examples
#' \donttest{
#' scatterplot(data = mtcars, x_var_name = "wt", y_var_name = "mpg")
#' scatterplot(
#'   data = mtcars, x_var_name = "wt", y_var_name = "mpg",
#'   point_label_var_name = "hp", weight_var_name = "drat",
#'   annotate_stats = TRUE
#' )
#' scatterplot(
#'   data = mtcars, x_var_name = "wt", y_var_name = "mpg",
#'   point_label_var_name = "hp", weight_var_name = "cyl",
#'   point_label_size = 7, annotate_stats = TRUE
#' )
#' }
#' @export
#' @import data.table ggplot2
scatterplot <- function(
  data = NULL,
  x_var_name = NULL,
  y_var_name = NULL,
  point_label_var_name = NULL,
  weight_var_name = NULL,
  alpha = 1,
  annotate_stats = FALSE,
  annotate_y_pos = 5,
  line_of_fit_type = "lm",
  ci_for_line_of_fit = FALSE,
  x_axis_label = NULL,
  y_axis_label = NULL,
  point_label_size = NULL,
  point_size_range = c(3, 12),
  jitter_x_percent = 0,
  jitter_y_percent = 0,
  cap_axis_lines = FALSE) {
  # create a temporary dataset
  dt01 <- data.table(x = data[[x_var_name]], y = data[[y_var_name]])
  # add the point label or weight column
  if (!is.null(point_label_var_name)) {
    dt01 <- data.table(
      dt01,
      point_labels = data[[point_label_var_name]]
    )
  }
  if (!is.null(weight_var_name)) {
    dt01 <- data.table(dt01, weight = data[[weight_var_name]])
  } else {
    # set weight as 1 if no weight_var_name is given
    dt01 <- data.table(dt01, weight = 1)
  }
  # remove na values
  dt02 <- stats::na.omit(dt01)
  if (nrow(dt02) < nrow(dt01)) {
    message(paste0(
      nrow(dt01) - nrow(dt02),
      " rows were removed because of missing values."
    ))
  }
  # ranges for x and y
  x_range <- max(dt02$x) - min(dt02$x)
  y_range <- max(dt02$y) - min(dt02$y)
  # start ggplot
  g1 <- ggplot(data = dt02, aes(x = dt02$x, y = dt02$y))
  # add jitter
  pj <- position_jitter(
    width = jitter_x_percent / 100 * x_range,
    height = jitter_y_percent / 100 * y_range
  )
  # add point labels or dots
  if (!is.null(point_label_var_name)) {
    g1 <- g1 + aes(label = dt02$point_labels)
    if (is.null(point_label_size)) {
      g1 <- g1 + geom_text(
        aes(label = dt02$point_labels, fontface = "bold"),
        position = pj)
    } else {
      g1 <- g1 + geom_text(
        aes(label = dt02$point_labels, fontface = "bold"),
        position = pj,
        size = point_label_size)
    }
  } else {
    g1 <- g1 + geom_point(position = pj)
  }
  # scale points
  if (!is.null(weight_var_name)) {
    g1 <- g1 + aes(size = dt02$weight)
    g1 <- g1 + scale_size(
      range = point_size_range, guide = FALSE)
  }
  # weighted least squares line
  if (line_of_fit_type %in% c("lm", "loess")) {
    g1 <- g1 + geom_smooth(
      formula = y ~ x,
      method = line_of_fit_type, mapping = aes(weight = dt02$weight),
      se = ci_for_line_of_fit
    )
  }
  # correlation
  cor_test <- stats::cor.test(dt02[["x"]], dt02[["y"]])
  cor_test_df <- cor_test[["parameter"]][["df"]]
  cor_test_r <- cor_test[["estimate"]]
  cor_test_p_value <- cor_test[["p.value"]]
  weighted_r_text <- ""
  # weighted correlation
  if (!is.null(weight_var_name)) {
    cor_test <-
      weights::wtd.cor(
        x = dt02$x, y = dt02$y,
        weight = dt02$weight
      )
    cor_test_r <- cor_test[1, "correlation"]
    cor_test_p_value <- cor_test[1, "p.value"]
    weighted_r_text <- "weighted"
  }
  # nice p value
  cor_test_p_value_text <-
    kim::pretty_round_p_value(cor_test_p_value, include_p_equals = TRUE)
  # annotate stats
  if (annotate_stats == TRUE) {
    annotation_01 <-
      as.character(as.expression(substitute(
        t06 * italic(t01)(t02) == t03 * t04 * italic(p) * t05,
        list(
          t01 = " r",
          t02 = cor_test_df,
          t03 = sub(
            "^(-?)0.", "\\1.",
            sprintf(paste0("%.", 2, "f"), cor_test_r)
          ),
          t04 = ", ",
          t05 = gsub("p", "", cor_test_p_value_text),
          t06 = weighted_r_text
        )
      )))
    g1 <- g1 + geom_text(
      aes(
        x = min(dt02$x) + x_range / 2,
        y = max(dt02$y) + y_range * annotate_y_pos / 100),
      color = "green4",
      label = annotation_01, parse = TRUE,
      hjust = 0.5, vjust = 0.5,
      size = 6,
      fontface = "bold"
    )
  }
  # axis labels
  if (is.null(x_axis_label)) {
    x_axis_label <- x_var_name
  }
  if (is.null(y_axis_label)) {
    y_axis_label <- y_var_name
  }
  g1 <- g1 + xlab(x_axis_label)
  g1 <- g1 + ylab(y_axis_label)
  # plot theme
  g1 <- g1 + kim::theme_kim(cap_axis_lines = cap_axis_lines)
  # return the ggplot
  return(g1)
}
