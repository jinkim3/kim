#' Akaike Weights
#'
#' Compare adequacy of different models by calculating their Akaike weights
#' and the associated evidence ratio.
#'
#' Please refer to Wagenmakers & Farrell (2004),
#' doi:10.3758/BF03206482
#'
#' @param aic_values a vector of AIC values
#' @param print_output_explanation logical. Should an explanation about
#' how to read the output be printed? (default = TRUE).
#' @return the output will be a data.table showing AIC weights,
#' their evidence ratio(s), etc.
#' @examples
#' # default reference AIC value is the minimum AIC value, e.g., 202 below.
#' akaike_weights(c(204, 202, 206, 206, 214))
#' @export
akaike_weights <- function(
    aic_values = NULL,
    print_output_explanation = TRUE) {
  # test values ----
  # aic_values <- c(3, 1, 2, - 1)
  # aic_values <- c(204, 202, 206, 206, 214)
  # ensure that the aic values have been entered ----
  if (is.null(aic_values)) {
    stop("Please enter AIC values.")
  }
  # ensure that the aic values are numeric ----
  if (!is.numeric(aic_values)) {
    stop("The AIC values must be numeric.")
  }
  # ensure that multiple aic values have been entered ----
  if (!length(aic_values) >= 2) {
    stop("Please enter two or more AIC values.")
  }
  # min aic ----
  min_aic <- min(aic_values)
  # compute the differences in aic with respect to the min aic ----
  # note: smaller aic values are better, even if they are negative
  # e.g., a model with aic = -237.847 is better than one with aic = -201.928
  aic_differences <- aic_values - min_aic
  # rel_lh stands for estimates of the relative likelihood of model i ----
  rel_lh <- exp(-1 / 2 * aic_differences)
  # obtain akaike weights ----
  aic_weights <- rel_lh / sum(exp(-1 / 2 * aic_differences))
  # multiplicative factor for likelihood of best model ----
  # mult_factor is short for multiplicative factor for
  # likelihood of best model
  min_aic_weight <- exp(-1 / 2 * (min_aic - min_aic)) /
    sum(exp(-1 / 2 * aic_differences))
  mult_factor <- min_aic_weight / aic_weights
  # evidence ratio ----
  evidence_ratio <- min_aic_weight / (aic_weights + min_aic_weight)
  # build a dt like table 1 of Wagenmakers & Farrell (2004) ----
  d1 <- data.table::data.table(
    model = as.character(seq_along(aic_values)),
    aic = aic_values,
    min_aic,
    aic_weight = aic_weights,
    mult_factor,
    evidence_ratio)
  # identify the model w the minimum aic value ----
  ref_model_row_number <- min(which(d1[["aic"]] == min_aic))
  data.table::set(
    d1, i = ref_model_row_number, j = "model", value =
      paste0("(min AIC) ", d1[["model"]][[ref_model_row_number]]))
  # print the explanation message
  if (print_output_explanation == TRUE) {
    row_number_for_example <- min(setdiff(
      seq_len(nrow(d1)), ref_model_row_number))
    mult_factor_for_example <- signif(
      d1[["mult_factor"]][[
        row_number_for_example]], 2)
    percent_to_prefer_for_example <- 100 * signif(
      d1[["evidence_ratio"]][[row_number_for_example]], 2)
    explanation_msg <- paste0(
      "Please interpret the tabulated values below as follows:\n\n",
      "The model with the minimum AIC value (Model ",
      ref_model_row_number,
      ") is ",
      mult_factor_for_example,
      " times more likely to be\n",
      "the best model in terms of Kullback-Leibler discrepancy ",
      "than is Model ",
      row_number_for_example,
      ".\nThere is a ",
      percent_to_prefer_for_example,
      "% probability that Model ",
      ref_model_row_number,
      " is to be preferred over Model ",
      row_number_for_example,
      ".\n\nSee Wagenmakers & Farrell (2006),\n",
      "https://doi.org/10.3758/BF03206482")
    message(explanation_msg)
  }
  output <- d1
  return(output)
}
