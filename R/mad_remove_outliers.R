#' Remove outliers using the MAD method
#'
#' Detect outliers in a numeric vector using the
#' Median Absolute Deviation (MAD) method and remove or convert them.
#' For more information on MAD, see Leys et al. (2013)
#' doi:10.1016/j.jesp.2013.03.013
#'
#' @param x a numeric vector
#' @param threshold the threshold value for determining outliers.
#' If \code{threshold == 2.5}, the median plus or minus 2.5 times the
#' MAD will be the cutoff values for determining outliers.
#' In other words, values less than the median minus 2.5
#' times the MAD and values greater than the median plus 2.5
#' times the MAD will be considered outliers.
#' By default, \code{threshold == 2.5}
#' @param constant scale factor for the 'mad' function in the 'stats'
#' package. It is the constant linked to the assumed distribution.
#' In case of normality, constant = 1.4826.
#' By default, \code{constant == 1.4826}.
#' @param convert_outliers_to the value to which outliers will be converted.
#' For example, if \code{convert_outliers_to = NA}, the outlier values will
#' be converted to NA values.
#' If \code{convert_outliers_to = 1000}, the outlier values will
#' be converted to 1000.
#' By default, \code{convert_outliers_to == NA}.
#' @param output_type type of the output.
#' If \code{output_type = "converted_vector"},
#' the function's output will be a vector with outliers converted to
#' the value set by the argument \code{convert_outliers_to}.
#' If \code{output_type = "outliers"}, the function's output will be
#' outliers in the original vector as determined by the MAD method.
#' If \code{output_type = "cutoff_values"}, the function's
#' output will be the cutoff values for determining outliers.
#' For example, if outliers will be values less than 0 and greater than 10,
#' the cutoff values will be 0 and 10.
#' If \code{output_type = "non_outlier_values"}, the function's output
#' will be a vector consisting only of the values that are not outliers;
#' here, the outliers will be removed from the vector, rather than
#' being converted to NA values.
#' By default, \code{output_type = "converted_vector"}.
#' @examples
#' \dontrun{
#' mad_remove_outliers(x = c(1, 3, 3, 6, 8, 10, 10, 1000))
#' mad_remove_outliers(x = c(1, 3, 3, 6, 8, 10, 10, 1000, -10000))
#' # return the vector with the outlier converted to NA values
#' mad_remove_outliers(
#' x = c(1, 3, 3, 6, 8, 10, 10, 1000, -10000),
#' output_type = "converted_vector")
#' # return the cutoff values for determining outliers
#' mad_remove_outliers(
#' x = c(1, 3, 3, 6, 8, 10, 10, 1000, -10000),
#' output_type = "cutoff_values")
#' # return the outliers
#' mad_remove_outliers(
#' x = c(1, 3, 3, 6, 8, 10, 10, 1000, -10000),
#' output_type = "outliers")
#' mad_remove_outliers(
#' x = c(1, 3, 3, 6, 8, 10, 10, 1000, -10000),
#' output_type = "non_outlier_values")
#' }
#' @export
mad_remove_outliers <- function(
  x = NULL,
  threshold = 2.5,
  constant = 1.4826,
  convert_outliers_to = NA,
  output_type = "converted_vector") {
  # ensure the vector is numeric
  if (is.numeric(x) == FALSE) {
    stop("The input x must be a numeric vector.")
  }
  # find mad
  mad <- stats::mad(x, constant = constant, na.rm = TRUE)
  # median
  x_median <- median(x, na.rm = TRUE)
  # cutoff values
  cutoff_low <- x_median - threshold * mad
  cutoff_high <- x_median + threshold * mad
  cutoff_values <- c(cutoff_low, cutoff_high)
  # return cutoff values
  if (output_type == "cutoff_values") {
    return(cutoff_values)
  }
  # return outliers
  if (output_type == "outliers") {
    outliers <- x[x < cutoff_low | x > cutoff_high]
    return(outliers)
  }
  # return values that are not outliers
  if (output_type == "non_outlier_values") {
    non_outlier_values <- x[x >= cutoff_low & x <= cutoff_high]
    return(non_outlier_values)
  }
  # convert outliers to na or another value
  if (output_type == "converted_vector") {
    x[x < cutoff_low | x > cutoff_high] <- convert_outliers_to
    return(x)
  }
}
