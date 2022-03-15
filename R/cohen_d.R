#' Calculate Cohen's d and its confidence interval using
#' the package 'psych'
#'
#' To run this function, the following package(s) must be installed:
#' Package 'psych' v2.1.9 (or possibly a higher version) by
#' William Revelle (2021),
#' <https://cran.r-project.org/package=psych>
#'
#' @param sample_1 a vector of values in the first of two samples
#' @param sample_2 a vector of values in the second of two samples
#' @param data a data object (a data frame or a data.table)
#' @param iv_name name of the independent variable
#' @param dv_name name of the dependent variable
#' @param ci_range range of the confidence interval for Cohen's d
#' (default = 0.95)
#' @param output_type If \code{output_type == "all"} or
#' if \code{output_type == "d_and_ci"}, the output will
#' be a vector of Cohen's d and its confidence interval.
#' If \code{output_type == "d"}, the output will be Cohen's d.
#' If \code{output_type == "ci"}, the output will be a vector
#' of the confidence interval around Cohen's d.
#' By default, \code{output_type == "all"}.
#' @examples
#' \dontrun{
#' cohen_d(sample_1 = 1:10, sample_2 = 3:12)
#' cohen_d(data = mtcars, iv_name = "vs", dv_name = "mpg", ci_range = 0.99)
#' sample_dt <- data.table::data.table(iris)[Species != "setosa"]
#' cohen_d(data = sample_dt, iv_name = "Species", dv_name = "Petal.Width")
#' }
#' @export
cohen_d <- function(
  sample_1 = NULL, sample_2 = NULL,
  data = NULL, iv_name = NULL, dv_name = NULL,
  ci_range = 0.95, output_type = "all") {
  # check if Package 'effsize' is installed
  if (!"psych" %in% rownames(utils::installed.packages())) {
    message(paste0(
      "To run this function, Package 'psych' must ",
      "be installed.\nTo install Package 'psych', type ",
      "'kim::prep(psych)'",
      "\n\nAlternatively, to install all packages (dependencies) required ",
      "for all\nfunctions in Package 'kim', type ",
      "'kim::install_all_dependencies()'"))
  }
  # proceed if Package 'psych' is already installed
  cohen_d_fn_from_psych <- utils::getFromNamespace(
    "cohen.d", "psych")
  # bind the vars locally to the function
  iv <- dv <- NULL
  # check arguments
  if (!is.null(sample_1) & !is.null(sample_2)) {
    if (is.numeric(sample_1) & is.numeric(sample_2)) {
      dt <- data.table::data.table(
        "iv" = c(
          rep(1, length(sample_1)),
          rep(2, length(sample_2))
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
        "Cohen's d can be calculated when there are exactly 2 levels in",
        " the independent variable."
      ))
    } else {
      dt <- data.table::data.table(
        "iv" = data[[iv_name]],
        "dv" = data[[dv_name]])
      # convert iv to numeric if necessary
      if (is.numeric(dt[, iv]) == FALSE) {
        dt[, iv := as.numeric(factor(iv))]
      }
    }
  }
  # deal w na rows
  dt <- stats::na.omit(dt)
  # calculate d
  cohen_d_results <- cohen_d_fn_from_psych(
    dv ~ iv, alpha = 1 - ci_range, data = dt)
  cohen_d <- cohen_d_results$cohen.d[, "effect"]
  # ci of d
  cohen_d_ci_ll <- cohen_d_results$cohen.d[, "lower"]
  cohen_d_ci_ul <- cohen_d_results$cohen.d[, "upper"]
  # output
  if (output_type %in% c("all", "d_and_ci")) {
    output <- c(cohen_d, cohen_d_ci_ll, cohen_d_ci_ul)
    names(output) <- c(
      "cohen_d",
      paste0("ci_", ci_range * 100, "_ll"),
      paste0("ci_", ci_range * 100, "_ul"))
  } else if (output_type == "d") {
    output <- cohen_d
    names(output) <- "cohen_d"
  } else if (output_type == "ci") {
    output <- c(cohen_d_ci_ll, cohen_d_ci_ul)
    names(output) <- c(
      paste0("ci_", ci_range * 100, "_ll"),
      paste0("ci_", ci_range * 100, "_ul"))
  }
  return(output)
}
