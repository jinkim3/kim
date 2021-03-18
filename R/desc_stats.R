#' Descriptive statistics
#'
#' Returns descriptive statistics for a numeric vector.
#'
#' @param vector a numeric vector
#' @param output_type if \code{output_type = "vector"}, return a vector
#' of descriptive statistics; if \code{output_type = "dt"}, return a
#' data.table of descriptive statistics (default = "vector")
#' @param sigfigs number of significant digits to round to (default = 3)
#' @param ci logical. Should 95% CI be included in the descriptive stats?
#' (default = TRUE)
#' @param pi logical. Should 95% PI be included in the descriptive stats?
#' (default = TRUE)
#' @param notify_na_count if \code{TRUE}, notify how many observations
#' were removed due to missing values. By default, NA count will be printed
#' only if there are any NA values.
#' @param print_dt if \code{TRUE}, print the descriptive stats data.table
#' @return if \code{output_type = "vector"}, the output will be a
#' named numeric vector of descriptive statistics;
#' if \code{output_type = "dt"}, the output will be data.table of
#' descriptive statistics.
#' @examples
#' desc_stats(1:100)
#' desc_stats(1:100, ci = TRUE, pi = TRUE, sigfigs = 2)
#' desc_stats(c(1:100, NA))
#' desc_stats(vector = c(1:100, NA), output_type = "dt")
#' @export
desc_stats <- function(
  vector = NULL,
  output_type = "vector",
  sigfigs = 3,
  ci = TRUE,
  pi = TRUE,
  notify_na_count = NULL,
  print_dt = TRUE) {
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
  se_of_mean <- se_of_mean(v_no_na, notify_na_count = FALSE)
  if (ci == TRUE) {
    ci_95_ll <- tryCatch(
      as.numeric(stats::t.test(v_no_na)[["conf.int"]][1]),
      warning = function(w) NA_real_, error = function(e) NA_real_)
    ci_95_ul <- tryCatch(
      as.numeric(stats::t.test(v_no_na)[["conf.int"]][2]),
      warning = function(w) NA_real_, error = function(e) NA_real_)
  }
  if (pi == TRUE) {
    pi_95_ll <- tryCatch(
      as.numeric(mean(v_no_na) + stats::sd(v_no_na) *
                   stats::qt(0.025, length(v_no_na) - 1)),
      warning = function(w) NA_real_, error = function(e) NA_real_)
    pi_95_ul <- tryCatch(
      as.numeric(mean(v_no_na) + stats::sd(v_no_na) *
          stats::qt(0.975, length(v_no_na) - 1)),
      warning = function(w) NA_real_, error = function(e) NA_real_)
  }
  skewness <- moments::skewness(v_no_na)
  kurtosis <- moments::kurtosis(v_no_na)
  # stats to report
  stats_to_report <- list(
    n = n,
    mean = mean,
    sd = sd,
    median = median,
    min = min,
    max = max,
    se_of_mean = se_of_mean)
  # add ci
  if (ci == TRUE) {
    stats_to_report <- c(
      stats_to_report,
      ci_95_ll = ci_95_ll,
      ci_95_ul = ci_95_ul)
  }
  # add pi
  if (pi == TRUE) {
    stats_to_report <- c(
      stats_to_report,
      pi_95_ll = pi_95_ll,
      pi_95_ul = pi_95_ul)
  }
  # add skewness and kurtosis
  stats_to_report <- c(
    stats_to_report,
    skewness = skewness,
    kurtosis = kurtosis)
  # stats in a data table format
  dt <- setDT(stats_to_report)
  # round
  stats_to_round <- c(
    "mean", "sd", "median", "se_of_mean", "ci_95_ll", "ci_95_ul",
    "pi_95_ll", "pi_95_ul", "skewness", "kurtosis")
  for (j in stats_to_round) {
    if (j %in% names(dt)) {
      data.table::set(
        dt, j = j,
        value = signif(dt[[j]], sigfigs))
    }
  }
  # print the data table
  if (print_dt == TRUE) {
    print(dt)
    cat("\n")
  }
  # return data table
  if (output_type == "dt") {
    invisible(dt)
  } else {
    output_vector <- vapply(dt, as.numeric, FUN.VALUE = numeric(1L))
    return(output_vector)
  }
}
