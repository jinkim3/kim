#' Descriptive statistics
#'
#' Returns descriptive statistics for a numeric vector.
#'
#' @param vector a numeric vector
#' @param output_type if \code{output_type = "vector"}, return a vector
#' of descriptive statistics; if \code{output_type = "dt"}, return a
#' data.table of descriptive statistics
#' @param sigfigs number of significant digits to round to (default = 3)
#' @param notify_na_count if \code{TRUE}, notify how many observations
#' were removed due to missing values. By default, NA count will be printed
#' only if there are any NA values.
#' @return a named numeric vector or a data.table
#' @examples
#' desc_stats(1:100)
#' desc_stats(c(1:100, NA))
#' desc_stats(vector = c(1:100, NA), output_type = "dt")
#' @export
desc_stats <- function(
  vector = NULL,
  output_type = "vector",
  sigfigs = 3,
  notify_na_count = NULL) {
  # deal with NA values
  v_no_na <- vector[!is.na(vector)]
  na_count <- length(vector) - length(v_no_na)
  # by default, notify only if NA values are present
  if (is.null(notify_na_count)) {
    notify_na_count <- ifelse(na_count > 0, TRUE, FALSE)
  }
  if (notify_na_count == TRUE) {
    message(paste0(
      "\n", na_count,
      " observation(s) were removed due to missing values.\n"
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
  # stats in a data table format
  dt <- data.table::data.table(
    n, mean, sd, median, min, max, se_of_mean, skewness, kurtosis)
  # round
  for (j in c(
    "mean", "sd", "median", "se_of_mean", "skewness", "kurtosis")) {
    data.table::set(
      dt, j = j,
      value = signif(dt[[j]], sigfigs))
  }
  # print the data table
  print(dt)
  cat("\n")
  # return data table
  if (output_type == "dt") {
    invisible(dt)
  } else {
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
}
