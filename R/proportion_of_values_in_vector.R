#' Proportion of given values in a vector
#'
#' @param vector a numeric or character vector containing
#' successes (hits) and failures (misses)
#' @param values a set of values that will count as successes (hits)
#' @param na.exclude if \code{TRUE}, NA values will be removed both from
#' \code{vector} and \code{values} before calculation (default = TRUE).
#' @param silent If \code{silent = TRUE}, no message will be printed
#' regarding number of NA values or confidence interval. (default = FALSE)
#' @param conf.level confidence level of the returned confidence interval.
#' Input to this argument will be passed onto the conf.level argument
#' in the \code{prop.test} function from the default stats package.
#' @param correct_yates a logical indicating whether Yates' continuity
#' correction should be applied where possible (default = TRUE).
#' Input to this argument will be passed onto the \code{correct} argument
#' in the \code{prop.test} function from the default stats package.
#' @param output_type By default, \code{output_type = "proportion"}.
#' If \code{output_type = "proportion"}, the function will return
#' the calculated proportion; if \code{output_type = "se"}, the function
#' will return the standard error of the sample proportion;
#' if \code{output_type = "dt"}, the function will return the
#' the data table of proportion and confidence intervals.
#' @examples
#' proportion_of_values_in_vector(
#'   values = 2:3, vector = c(rep(1:3, each = 10), rep(NA, 10))
#' )
#' proportion_of_values_in_vector(
#'   values = 2:3, vector = c(rep(1:3, each = 10), rep(NA, 10)),
#'   output_type = "se"
#' )
#' proportion_of_values_in_vector(
#'   values = 2:3, vector = c(rep(1:3, each = 10), rep(NA, 10)),
#'   conf.level = 0.99
#' )
#' proportion_of_values_in_vector(
#'   values = c(2:3, NA), vector = c(rep(1:3, each = 10), rep(NA, 10)),
#'   na.exclude = FALSE
#' )
#' @export
proportion_of_values_in_vector <- function(
  values = NULL,
  vector = NULL,
  na.exclude = TRUE,
  output_type = "proportion",
  silent = FALSE,
  conf.level = 0.95,
  correct_yates = TRUE) {
  if (silent == FALSE) {
    # notify of na values
    message(paste0(
      'NA values in the "values" argument: ',
      sum(is.na(values)), " out of ", length(values)
    ))
    message(paste0(
      'NA values in the "vector" argument: ',
      sum(is.na(vector)), " out of ", length(vector)
    ))
  }
  if (na.exclude == TRUE) {
    if (silent == FALSE) {
      message(paste0(
        'NA values were removed from the "values" and "vector"',
        " arguments before calculating the proportion."))
    }
    values_with_no_na <- values[!is.na(values)]
    vector_with_no_na <- vector[!is.na(vector)]
    # number of hits or number of successes
    successes <- sum(vapply(vector_with_no_na, function(x) {
      ifelse(x %in% values_with_no_na, TRUE, FALSE)
    }, FUN.VALUE = logical(1L)))
    # number of observations
    n <- length(vector_with_no_na)
    # ci
  } else {
    if (silent == FALSE) {
      message(paste0(
        'NA values were counted as valid values in the "values" and "vector"',
        " arguments when calculating the proportion."))
    }
    # number of hits or number of successes
    successes <- sum(vapply(vector, function(x) {
      ifelse(x %in% values, TRUE, FALSE)
    }, FUN.VALUE = logical(1L)))
    # number of observations
    n <- length(vector)
  }
  # proportion
  proportion <- successes / n
  # standard error se of sample proportion
  se <- sqrt(proportion * (1 - proportion) / n)
  names(se) <- "std_error_of_sample_proportion"
  # prop test results
  prop_test_results <- stats::prop.test(
    x = successes,
    n = n,
    conf.level = conf.level)
  # proportion and confidence interval
  proportion <- unname(prop_test_results[["estimate"]])
  conf_int <- unname(prop_test_results[["conf.int"]])
  # ci percent
  ci_percent <- conf.level * 100
  # print proportion and confidence interval
  if (silent == FALSE | output_type == "dt") {
    results_dt <- data.table::data.table(
      proportion = proportion,
      ci_ll = conf_int[1],
      ci_ul = conf_int[2])
    names(results_dt) <- c(
      "proportion",
      paste0("ci_", ci_percent, "_", c("ll", "ul")))
    # print table only if necessary
    if (silent == FALSE) {
      print(results_dt)
    }
  }
  # return by output type
  if (output_type == "proportion") {
    invisible(proportion)
  } else if (output_type == "se") {
    return(se)
  } else if (output_type == "dt") {
    invisible(results_dt)
  }
}
