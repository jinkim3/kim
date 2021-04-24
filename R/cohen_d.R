#' Cohen's d with confidence interval
#'
#' Calculates Cohen's d using the 'effsize' package v0.8.1 by
#' Torchiano (2020) <https://github.com/mtorchiano/effsize/>
#'
#' @param sample_1 a vector of values in the first of two samples
#' @param sample_2 a vector of values in the second of two samples
#' @param data a data object (a data frame or a data.table)
#' @param iv_name name of the independent variable
#' @param dv_name name of the dependent variable
#' @param ci_range range of the confidence interval for Cohen's d
#' (default = 0.95)
#' @examples
#' cohen_d(1:10, 3:12)
#' cohen_d(data = mtcars, iv_name = "vs", dv_name = "mpg", ci_range = 0.99)
#' @export
cohen_d <- function(
  sample_1 = NULL, sample_2 = NULL,
  data = NULL, iv_name = NULL, dv_name = NULL,
  ci_range = 0.95) {
  # check if Package 'effsize' is installed
  if (!"effsize" %in% rownames(utils::installed.packages())) {
    message(paste0(
      "To calculate the confidence interval of Cohen's d, ",
      "Package 'effsize' must ",
      "be installed.\nTo install Package 'effsize', type ",
      "'kim::prep(effsize)'\n"))
    output <- kim::cohen_d_from_cohen_textbook(
      sample_1 = sample_1, sample_2 = sample_2,
      data = data, iv_name = iv_name, dv_name = dv_name)
    return(output)
  }
  # proceed if Package 'effsize' is already installed
  cohen_d_fn_from_effsize <- utils::getFromNamespace(
      "cohen.d", "effsize")
  # check arguments
  if (!is.null(sample_1) & !is.null(sample_2)) {
    if (is.numeric(sample_1) & is.numeric(sample_2)) {
      df <- data.frame(
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
      df <- data.frame(
        "iv" = data[[iv_name]],
        "dv" = data[[dv_name]]
      )
    }
  }
  # convert iv to factor
  df$iv <- factor(df$iv)
  output <- cohen_d_fn_from_effsize(
    formula = dv ~ iv, data = df, conf.level = ci_range)
  return(output)
}
