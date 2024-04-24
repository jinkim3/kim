#' Confidence Interval of the Mean of a Vector
#'
#' Returns the confidence interval of the mean of a numeric vector.
#'
#' @param x a numeric vector
#' @param confidence_level What is the desired confidence level
#' expressed as a decimal? (default = 0.95)
#' @param notify_na_count if \code{TRUE}, notify how many observations
#' were removed due to missing values. By default, NA count will be printed
#' only if there are any NA values.
#' @return the output will be a named numeric vector with the
#' lower and upper limit of the confidence interval.
#' @examples
#' ci_of_mean(x = 1:100, confidence_level = 0.95)
#' ci_of_mean(mtcars$mpg)
#' @export
ci_of_mean <- function(
  x = NULL,
  confidence_level = 0.95,
  notify_na_count = NULL) {
  # deal with NA values
  x_no_na <- x[!is.na(x)]
  na_count <- length(x) - length(x_no_na)
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
  ci_ll <- tryCatch(
    as.numeric(stats::t.test(
      x_no_na, conf.level = confidence_level)[["conf.int"]][1]),
    warning = function(w) NA_real_, error = function(e) NA_real_)
  ci_ul <- tryCatch(
    as.numeric(stats::t.test(
      x_no_na, conf.level = confidence_level)[["conf.int"]][2]),
    warning = function(w) NA_real_, error = function(e) NA_real_)
  output <- c(ci_ll, ci_ul)
  names(output) <- c(
    paste0("ci_", confidence_level * 100, "_ll"),
    paste0("ci_", confidence_level * 100, "_ul"))
  return(output)
}
