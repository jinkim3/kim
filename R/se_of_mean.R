#' Standard error of the mean
#'
#' @param vector a numeric vector
#' @param na.rm if \code{TRUE}, NA values will be removed before calculation
#' @param notify_na_count if \code{TRUE}, notify how many observations
#' were removed due to missing values. By default, NA count will be printed
#' only if there are any NA values.
#' @return the output will be a numeric vector of length one,
#' which will be the standard error of the mean for the given numeric vector.
#' @examples
#' se_of_mean(c(1:10, NA))
#' @export
se_of_mean <- function(vector, na.rm = TRUE, notify_na_count = NULL) {
  x <- vector[!is.na(vector)]
  # by default, notify only if NA values are present
  if (is.null(notify_na_count)) {
    notify_na_count <- ifelse(length(vector) - length(x) > 0, TRUE, FALSE)
  }
  if (notify_na_count == TRUE) {
    message(paste0(
      length(vector) - length(x),
      " observations were removed due to missing values."
    ))
  }
  output <- stats::sd(x) / sqrt(length(x))
  return(output)
}
