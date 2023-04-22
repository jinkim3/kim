#' Binomial test
#'
#' Conduct a binomial test. In other words, test whether an observed
#' proportion of "successes" (e.g., proportion of heads in a series
#' of coin tosses) is greater than the expected proportion (e.g., 0.5).
#' This function uses the 'binom.test' function from the 'stats' package.
#'
#' @param x a vector of values, each of which represents an instance of
#' either a "success" or "failure" (e.g., c("s", "f", "s", "s", "f", "s"))
#' @param success which value(s) indicate "successes"?
#' @param failure (optional) which value(s) indicate "failures"?
#' If no input is provided for this argument, then all the non-NA values
#' that are not declared to be "successes" will be treated as "failures".
#' @param p hypothesized probability of success (default = 0.5)
#' @param alternative indicates the alternative hypothesis and must be
#' one of "two.sided", "greater", or "less". You can specify just the
#' initial letter. By default, \code{alternative = "two.sided"}
#' @param ci width of the confidence interval (default = 0.95)
#' @param round_percentages number of decimal places to which to round the
#' percentages in the summary table (default = 0)
#' @examples
#' # sample vector
#' sample_vector <- c(0, 1, 1, 0, 1, 98, 98, 99, NA)
#' binomial_test(
#' x = sample_vector,
#' success = 1, failure = 0)
#' binomial_test(
#' x = sample_vector,
#' success = 1, failure = 0,
#' p = 0.1,
#' alternative = "greater")
#' binomial_test(
#' x = sample_vector,
#' success = c(1, 99), failure = c(0, 98),
#' p = 0.6,
#' alternative = "less")
#' @export
binomial_test <- function(
  x = NULL,
  success = NULL,
  failure = NULL,
  p = 0.5,
  alternative = "two.sided",
  ci = 0.95,
  round_percentages = 0
) {
  # bind the vars locally to the function
  count <- percent <- value <- NULL
  # check inputs
  if (is.null(x)) {
    stop("Please provide an input for the argument 'x'.")
  }
  if (is.null(success)) {
    stop("Please provide an input for the argument 'success'.")
  }
  # remove na values
  x2 <- stats::na.omit(x)
  if (length(x2) < length(x)) {
    message(paste0(
      length(x) - length(x2),
      " NA value(s) were removed before conducting the binomial test.\n"))
  }
  # set values for failure
  if (is.null(failure)) {
    failure <- setdiff(x2, success)
  }
  # number of successes
  num_of_successes <- sum(x %in% success, na.rm = TRUE)
  # number of failures
  num_of_failures <- sum(x %in% failure, na.rm = TRUE)
  # number of successes and failures
  num_of_successes_and_failures <- num_of_successes + num_of_failures
  # summarize in a data table
  summary_table <- data.table::data.table(
    instance = c("success", "failure"),
    " " = " ")
  summary_table[, value := c(
    paste0(success, collapse = ", "),
    paste0(failure, collapse = ", "))]
  summary_table[, count := c(
    num_of_successes, num_of_failures)]
  summary_table[, percent := round(
    count / num_of_successes_and_failures * 100, round_percentages)]
  # print the summary table
  print(summary_table)
  # binom test
  stats::binom.test(
    x = c(num_of_successes, num_of_failures),
    p = p,
    alternative = alternative,
    conf.level = ci)
}
