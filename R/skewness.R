#' Skewness
#'
#' Calculate skewness using one of three formulas: (1) the traditional
#' Fisher-Pearson coefficient of skewness; (2) the adjusted
#' Fisher-Pearson standardized moment coefficient; (3) the Pearson 2
#' skewness coefficient. Formulas were taken from Doane & Seward (2011),
#' \doi{10.1080/10691898.2011.11889611}
#'
#' @param vector a numeric vector
#' @param type a character string indicating the type of skewness to
#' calculate. If \code{type = "adjusted"}, the adjusted Fisher-Pearson
#' standardized moment coefficient will be calculated. If
#' \code{type = "traditional"}, the traditional Fisher-Pearson
#' coefficient of skewness will be calculated. If \code{type = "pearson_2"},
#' the Pearson 2 skewness coefficient will be calculated. By default,
#' \code{type = "adjusted"}.
#' @return a numeric value, i.e., skewness of the given vector
#' @examples
#' # calculate the adjusted Fisher-Pearson standardized moment coefficient
#' kim::skewness(c(1, 2, 3, 4, 5, 10))
#' # calculate the traditional Fisher-Pearson coefficient of skewness
#' kim::skewness(c(1, 2, 3, 4, 5, 10), type = "traditional")
#' # compare with skewness from 'moments' package
#' moments::skewness(c(1, 2, 3, 4, 5, 10))
#' # calculate the Pearson 2 skewness coefficient
#' kim::skewness(c(1, 2, 3, 4, 5, 10), type = "pearson_2")
#' @export
skewness <- function(
  vector = NULL,
  type = "adjusted") {
  # throw an error if no vector is given
  if (is.null(vector)) {
    stop("Please enter a vector for which to calculate skewness.")
  }
  # throw an error if vector is not numeric
  if (!is.numeric(vector)) {
    stop("Please enter a numeric vector for which to calculate skewness.")
  }
  # throw an error if type is not one of the three preset types
  if (!(type %in% c("adjusted", "traditional", "pearson_2"))) {
    stop(paste0(
      "The type of skewness must be one of the following: ",
      '"adjusted", "traditional", or "pearson_2".'))
  }
  # remove NA values
  x <- vector[!is.na(vector)]
  # calculate stats
  x_bar <- mean(x)
  x_median <- stats::median(x)
  s <- stats::sd(x)
  n <- length(x)
  # calculate the adjusted Fisher-Pearson standardized moment coefficient
  if (type == "adjusted") {
    adjusted_skewness <- (n / ((n - 1) * (n - 2))) *
      sum(((x - x_bar) / s) ^ 3)
    return(adjusted_skewness)
  }
  # calculate the traditional Fisher-Pearson coefficient of skewness
  if (type == "traditional") {
    traditional_skewness <- ((1 / n) * sum((x - x_bar) ^ 3)) /
      ((1 / n) * sum((x - x_bar) ^ 2)) ^ (3 / 2)
    return(traditional_skewness)
  }
  # calculate the Pearson 2 skewness coefficient
  if (type == "pearson_2") {
    pearson_2_skewness <- 3 * ((x_bar - x_median) / s)
    return(pearson_2_skewness)
  }
}
