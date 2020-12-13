#' Descriptive statistics
#'
#' Returns descriptive statistics for a numeric vector.
#'
#' @param vector a numeric vector
#' @param notify_na_count if \code{TRUE}, notify how many observations
#' were removed due to missing values. By default, NA count will be printed
#' only if there are any NA values.
#' @return a named numeric vector
#' @examples
#' desc_stats(1:100)
#' desc_stats(c(1:100, NA))
#' @export
desc_stats <- function(vector = NULL, notify_na_count = NULL) {
  # deal with NA values
  v_no_na <- vector[!is.na(vector)]
  na_count <- length(vector) - length(v_no_na)
  # by default, notify only if NA values are present
  if (is.null(notify_na_count)) {
    notify_na_count <- ifelse(na_count > 0, TRUE, FALSE)
  }
  if (notify_na_count == TRUE) {
    message(paste0(
      na_count, " observation(s) were removed due to missing values."
    ))
  }
  # get stats
  n <- length(v_no_na)
  mean <- mean(v_no_na)
  sd <- stats::sd(v_no_na)
  median <- stats::median(v_no_na)
  min <- min(v_no_na)
  max <- max(v_no_na)
  se_of_mean <- kim::se_of_mean(v_no_na, notify_na_count = FALSE)
  skewness <- moments::skewness(v_no_na)
  kurtosis <- moments::kurtosis(v_no_na)
  # combine into a list
  stats_list <- list(
    n, mean, sd, median, min, max, se_of_mean, skewness, kurtosis)
  statistic <- vapply(stats_list, as.numeric, FUN.VALUE = numeric(1L))
  names(statistic) <- c(
    "n", "mean", "sd", "median", "min", "max",
    "se_of_mean", "skewness", "kurtosis"
  )
  return(statistic)
}
