#' Histogram by group
#'
#' Creates histograms by group to compare distributions
#'
#' @param data a data object (a data frame or a data.table)
#' @param iv_name name of the independent variable
#' @param dv_name name of the dependent variable
#' @param order_of_groups_top_to_bot a character vector indicating
#' the desired presentation order of levels in the independent variable
#' (from the top to bottom). Omitting a group in this argument will
#' remove the group in the set of histograms.
#' @param number_of_bins number of bins for the histograms (default = 40)
#' @param space_between_histograms space between histograms
#' (minimum = 0, maximum = 1, default = 0.15)
#' @param draw_baseline logical. Should the baseline and the trailing
#' lines to either side of the histogram be drawn? (default = FALSE)
#' @return the output will be a set of vertically arranged histograms
#' (a ggplot object), i.e., one histogram for each level of the
#' independent variable.
#' @examples
#' histogram_by_group(data = mtcars, iv_name = "cyl", dv_name = "mpg")
#' histogram_by_group(
#'   data = mtcars, iv_name = "cyl", dv_name = "mpg",
#'   order_of_groups_top_to_bot = c("8", "4"), number_of_bins = 10,
#'   space_between_histograms = 0.5
#' )
#' histogram_by_group(
#' data = iris, iv_name = "Species", dv_name = "Sepal.Length")
#' @export
#' @import data.table ggplot2
histogram_by_group <- function(
  data = NULL,
  iv_name = NULL,
  dv_name = NULL,
  order_of_groups_top_to_bot = NULL,
  number_of_bins = 40,
  space_between_histograms = 0.15,
  draw_baseline = FALSE) {
  # create the dataset
  dt01 <- stats::na.omit(
    data.table::setDT(
      data.table::copy(data))[, c(iv_name, dv_name), with = FALSE]
  )
  # change names to just iv and dv
  names(dt01) <- c("iv", "dv")
  # if iv is numeric, change it to character
  if (is.numeric(dt01$iv)) {
    dt01$iv <- as.character(dt01$iv)
  }
  # order groups
  if (is.null(order_of_groups_top_to_bot)) {
    order_of_groups_top_to_bot <- sort(unique(dt01$iv))
  }
  # convert order argument to character
  order_of_groups_top_to_bot <- as.character(order_of_groups_top_to_bot)
  # only the groups specified in iv_order
  dt01 <- dt01[get("iv") %in% order_of_groups_top_to_bot]
  # means and medians
  stats_by_iv <-
    dt01[, list(
      mean = mean(get("dv")),
      n = length(get("dv")),
      se_of_mean = stats::sd(get("dv")) / sqrt(length(get("dv"))),
      # ran into an error without the "as.double" below
      median = as.double(stats::median(get("dv")))
    ),
    keyby = "iv"
    ]
  # order the stats table
  stats_by_iv <- stats_by_iv[
    match(
      make.unique(order_of_groups_top_to_bot),
      make.unique(as.character(stats_by_iv[["iv"]]))
    )
  ]
  reversed_order <- rev(order_of_groups_top_to_bot)
  y_tick_mark_labels <- vapply(reversed_order, function(x) {
    paste0(
      x, "  \n(n = ", stats_by_iv[stats_by_iv$iv == x]$n,
      ")  "
    )
  },
  FUN.VALUE = character(1L)
  )
  # begin plotting
  g1 <- ggplot(
    data = dt01,
    aes(
      x = get("dv"), y = get("iv"),
      stat(stats::density), fill = get("iv")
    )
  ) +
    ggridges::geom_density_ridges2(
      stat = "binline", bins = number_of_bins,
      scale = (1 - space_between_histograms),
      draw_baseline = draw_baseline
    )
  g1 <- g1 + scale_y_discrete(
    limits = reversed_order,
    breaks = reversed_order,
    labels = y_tick_mark_labels
  )
  g1 <- g1 + scale_x_continuous(expand = c(0, 0))
  g1 <- g1 + theme_classic(base_size = 16) %+replace%
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text = element_text(
        face = "bold", color = "black", size = 12, hjust = 0.5
      ),
      axis.text.y = element_text(hjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 12)),
      axis.title.y = element_text(
        vjust = 0.95,
        margin = margin(r = 12)
      ),
      legend.position = "none"
    )
  g1 <- g1 + coord_cartesian(clip = "off")
  g1 <- g1 + xlab(dv_name)
  g1 <- g1 + ylab(iv_name)
  g1 <- g1 + geom_point(
    data = stats_by_iv, aes(
      x = stats_by_iv$mean, y = stats_by_iv$iv
    ), size = 4
  )
  g1 <- g1 + geom_errorbarh(
    data = stats_by_iv,
    aes(
      xmin = stats_by_iv$mean - stats_by_iv$se_of_mean,
      xmax = stats_by_iv$mean + stats_by_iv$se_of_mean,
      y = stats_by_iv$iv
    ),
    size = 2, height = 0.2, inherit.aes = FALSE
  )
  # medians
  g1 <- g1 + geom_text(
    data = stats_by_iv,
    aes(
      x = stats_by_iv$median, y = stats_by_iv$iv, label = "Mdn\nX",
      fontface = 2
    ), vjust = -0.5
  )
  return(g1)
}
