#' Cohen's d
#'
#' Calculates Cohen's d, its standard error, and confidence interval.
#' See Borenstein et al. (2009, ISBN: 978-0-470-05724-7).
#'
#' @param sample_1 a vector of values in the first of two samples
#' @param sample_2 a vector of values in the second of two samples
#' @param data a data object (a data frame or a data.table)
#' @param iv_name name of the independent variable
#' @param dv_name name of the dependent variable
#' @param ci_range range of the confidence interval for Cohen's d
#' (default = 0.95)
#' @param direction If \code{direction == "2_minus_1"}, Cohen's d will
#' reflect the extent to which the mean of IV level 2 is greater than
#' the mean of IV level 2. If \code{direction == "1_minus_2"}, Cohen's d
#' will reflect the extent to which the mean of IV level 1 is greater than
#' the mean of IV level 2. By default, \code{direction == "2_minus_1"}.
#' @param output_type If \code{output_type == "all"} or
#' if \code{output_type == "d_var_se_and_ci"}, the output will
#' be a vector of Cohen's d and its variance, SE, and confidence interval.
#' If \code{output_type == "d_se_and_ci"}, the output will
#' be a vector of Cohen's d and its SE and confidence interval.
#' If \code{output_type == "d_and_ci"}, the output will
#' be a vector of Cohen's d and its confidence interval.
#' If \code{output_type == "d"}, the output will be Cohen's d.
#' If \code{output_type == "ci"}, the output will be a vector
#' of the confidence interval around Cohen's d.
#' If \code{output_type == "se"}, the output will be the standard error
#' of Cohen's d.
#' By default, \code{output_type == "all"}.
#' @param initial_value initial value of the noncentrality parameter for
#' optimization (default = 0). Adjust this value if confidence
#' interval results look strange.
#' @examples
#' \donttest{
#' cohen_d(sample_1 = 1:10, sample_2 = 3:12)
#' cohen_d(data = mtcars, iv_name = "vs", dv_name = "mpg", ci_range = 0.99)
#' }
#' @export
cohen_d <- function(
  sample_1 = NULL, sample_2 = NULL,
  data = NULL, iv_name = NULL, dv_name = NULL,
  direction = "2_minus_1",
  ci_range = 0.95, output_type = "all",
  initial_value = 0) {
  # bind the vars locally to the function
  iv <- dv <- NULL
  # check arguments
  if (!is.null(sample_1) & !is.null(sample_2)) {
    if (is.numeric(sample_1) & is.numeric(sample_2)) {
      dt <- data.table::data.table(
        "iv" = c(
          rep("sample_1", length(sample_1)),
          rep("sample_2", length(sample_2))
        ),
        "dv" = c(sample_1, sample_2)
      )
    } else {
      stop(paste0(
        "Please make sure that both of the vectors, sample_1 ",
        "and sample_2 are numeric vectors."
      ))
    }
  }
  # if data object is provided
  if (!is.null(data) & !is.null(iv_name) & !is.null(dv_name)) {
    if (length(unique(data[[iv_name]])) != 2) {
      stop(paste0(
        "The independent variable has ",
        length(unique(data[[iv_name]])), " levels.\n",
        "Cohen's d can be calculated when there are exactly 2 levels."
      ))
    } else {
      dt <- data.table::data.table(
        "iv" = data[[iv_name]],
        "dv" = data[[dv_name]])
    }
  }
  # deal w na rows
  dt <- stats::na.omit(dt)
  # levels of iv
  if (is.null(levels(dt[["iv"]]))) {
    iv_level_1 <- kim::su(dt[["iv"]])[1]
    iv_level_2 <- kim::su(dt[["iv"]])[2]
  } else {
    iv_level_1 <- levels(dt[["iv"]])[1]
    iv_level_2 <- levels(dt[["iv"]])[2]
  }
  # vectors
  v1 <- dt[iv == iv_level_1, dv]
  v2 <- dt[iv == iv_level_2, dv]
  # group means
  mean_1 <- mean(v1, na.rm = TRUE)
  mean_2 <- mean(v2, na.rm = TRUE)
  # n
  n_1 <- sum(!is.na(v1))
  n_2 <- sum(!is.na(v1))
  # sd
  sd_1 <- stats::sd(v1, na.rm = TRUE)
  sd_2 <- stats::sd(v2, na.rm = TRUE)
  # the within-groups sd, pooled across groups
  s_within <- sqrt(
    ((n_1 - 1) * sd_1 ^ 2 + (n_2 - 1) * sd_2 ^ 2) / (n_1 + n_2 - 2))
  # d
  if (direction == "2_minus_1") {
    d <- (mean_2 - mean_1) / s_within
  } else if (direction == "1_minus_2") {
    d <- (mean_1 - mean_2) / s_within
  }
  # "the variance of d (to a very good approximation)"
  # p.27 of Borenstein et al. (2009)
  v_d <- (n_1 + n_2) / (n_1 * n_2) + d ^ 2 / (2 * (n_1 + n_2))
  # standard error of d
  se_d <- sqrt(v_d)
  # t test for finding ncp
  t_test_results <- stats::t.test(formula = dv ~ iv, data = dt)
  # find noncentrality parameters
  ncp_values <- kim::noncentrality_parameter(
    t_stat = t_test_results$statistic,
    df = t_test_results$parameter[["df"]],
    ci = ci_range,
    initial_value = initial_value)
  # ci of d
  ci <- ncp_values * sqrt(1 / n_1 + 1 / n_2)
  names(ci) <- c(paste0("ci_", paste0(ci_range * 100), "_ll"),
                 paste0("ci_", paste0(ci_range * 100), "_ul"))
  # output
  if (output_type %in% c("all", "d_var_se_and_ci")) {
    message("Standard error of Cohen's d is approximate.")
    output <- c(cohen_d = d, v_d = v_d, se_d = se_d, ci)
  } else if (output_type %in% c("d_se_and_ci")) {
    message("Standard error of Cohen's d is approximate.")
    output <- c(cohen_d = d, se_d = se_d, ci)
  } else if (output_type == "d_and_ci") {
    output <- c(cohen_d = d, ci)
  } else if (output_type == "d") {
    output <- c(cohen_d = d)
  } else if (output_type == "ci") {
    output <- ci
  } else if (output_type == "se") {
    message("Standard error of Cohen's d is approximate.")
    output <- c(se_d = se_d)
  }
  return(output)
}
