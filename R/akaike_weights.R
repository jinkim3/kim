#' Akaike Weights
#'
#' Compare adequacy of different models by calculating their Akaike weights
#' and the associated evidence ratio.
#'
#' Please refer to Wagenmakers & Farrell (2004),
#' doi:10.3758/BF03206482
#'
#' @param aic_values a vector of AIC values
#' @param ref_aic reference AIC value(s). This could be a single AIC value,
#' or a vector of AIC values with the same length as \code{aic_values}.
#' @param print_output_explanation logical. Should an explanation about
#' how to read the output be printed? (default = TRUE).
#' @return the output will be a data.table showing AIC weights,
#' their evidence ratio(s), etc.
#' @examples
#' # default reference AIC value is the minimum AIC value, e.g., 202 below.
#' akaike_weights(c(204, 202, 206, 206, 214))
#' akaike_weights(
#' aic_values = c(204, 202, 206, 206, 214),
#' ref_aic = 201:205)
#' @export
akaike_weights <- function(
  aic_values = NULL,
  ref_aic = NULL,
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
  # reference aic ----
  if (is.null(ref_aic)) {
    ref_aic <- min(aic_values)
  }
  # ensure the reference aic values have a proper length ----
  if (length(ref_aic) %in% c(1, length(aic_values)) == FALSE) {
    stop(paste0("The reference AIC values must be either a single value ",
         "or a vector that has the same length as `aic_values`."))
  }
  # compute the differences in aic with respect to the reference aic ----
  # note: smaller aic values are better, even if they are negative
  # e.g., a model with aic = -237.847 is better than one with aic = -201.928
  aic_differences <- aic_values - ref_aic
  # rel_lh stands for estimates of the relative likelihood of model i ----
  rel_lh <- exp(-1 / 2 * aic_differences)
  # obtain akaike weights ----
  aic_weights <- rel_lh / sum(exp(-1 / 2 * aic_differences))
  # multiplicative factor for likelihood of best model ----
  # mult_factor is short for multiplicative factor for
  # likelihood of best model
  ref_aic_weight <- exp(-1 / 2 * (ref_aic - ref_aic)) /
    sum(exp(-1 / 2 * aic_differences))
  mult_factor <- ref_aic_weight / aic_weights
  # evidence ratio ----
  evidence_ratio <- ref_aic_weight / (aic_weights + ref_aic_weight)
  # build a dt like table 1 of Wagenmakers & Farrell (2004) ----
  d1 <- data.table::data.table(
    model = as.character(seq_along(aic_values)),
    aic = aic_values,
    ref_aic = ref_aic,
    aic_weight = aic_weights,
    ref_model_is_x_times_better = mult_factor,
    evidence_ratio)
  # identify the reference model ----
  if (length(ref_aic) == 1) {
    ref_model_row_number <- min(which(d1[["aic"]] == ref_aic))
    data.table::set(
      d1, i = ref_model_row_number, j = "model", value =
        paste0("(reference) ", d1[["model"]][[ref_model_row_number]]))
    # print the explanation message
    if (print_output_explanation == TRUE) {
      row_number_for_example <- min(setdiff(
        seq_len(nrow(d1)), ref_model_row_number))
      mult_factor_for_example <- signif(
        d1[["ref_model_is_x_times_better"]][[
          row_number_for_example]], 2)
      percent_to_prefer_for_example <- 100 * signif(
        d1[["evidence_ratio"]][[row_number_for_example]], 2)
      explanation_msg <- paste0(
        "Please interpret the values below as follows:\n\n",
        "The reference model (Model ",
        ref_model_row_number,
        ") is ",
        mult_factor_for_example,
        " times more likely\n",
        "to be the best model in terms of Kullback Leibler discrepancy\n",
        "than is Model ",
        row_number_for_example,
        ",\nand there is a ",
        percent_to_prefer_for_example,
        "% probability that the reference model is\n",
        "to be preferred over Model ",
        row_number_for_example,
        ".\n\nSee Wagenmakers & Farrell (2006),\n",
        "https://doi.org/10.3758/BF03206482")
      message(explanation_msg)
    }
  }
  # # print the reference aic ----
  # if (length(ref_aic) == 1) {
  #   message(paste0("Reference AIC: ", ref_aic))
  # }
  # output ----
  output <- d1
  return(output)
}
