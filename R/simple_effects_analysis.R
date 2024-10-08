#' Simple Effects Analysis
#'
#' Conduct a simple effects analysis to probe a two-way interaction effect.
#' See Field et al. (2012, ISBN: 978-1-4462-0045-2).
#'
#' @param data a data object (a data frame or a data.table)
#' @param dv_name name of the dependent variable (DV)
#' @param iv_1_name name of the first independent variable (IV1), whose
#' main effects will be examined in the first set of contrasts
#' @param iv_2_name name of the second independent variable (IV2), whose
#' simple effects at each level of IV1 will be examined in the second set
#' of contrasts
#' @param iv_1_levels ordered levels of IV1
#' @param iv_2_levels ordered levels of IV2
#' @param print_contrast_table If
#' \code{print_contrast_table = "weights_sums_and_products"}, contrasts'
#' weights, sums of the weights and products will be printed. If
#' \code{print_contrast_table = "weights_only"}, only the contrasts will
#' be printed.
#' @param output output can be one of the following: \code{"lm_object"},
#' \code{"table"}, \code{"weights_only"}, \code{"weights_sums_and_products"},
#' \code{"all"} By default, \code{output = NULL}, and there will be no
#' output from the function other than the tables of simple effects and
#' constrasts which will be printed on the console by default.
#' @return By default, the function will print a table of contrasts and
#' a table of simple effects.
#' @examples
#' \donttest{
#' factorial_anova_2_way(
#'   data = mtcars, dv_name = "mpg", iv_1_name = "vs",
#'   iv_2_name = "am", iterations = 100, plot = TRUE)
#' simple_effects_analysis(
#'   data = mtcars, dv_name = "mpg", iv_1_name = "vs",
#'   iv_2_name = "am")
#' }
#' @export
simple_effects_analysis <- function(
  data = NULL,
  dv_name = NULL,
  iv_1_name = NULL,
  iv_2_name = NULL,
  iv_1_levels = NULL,
  iv_2_levels = NULL,
  print_contrast_table = "weights_sums_and_products",
  output = NULL
) {
  # bind the vars locally to the function
  iv1 <- iv2 <- dv <- simple <- NULL
  # convert data to data table
  dt1 <- data.table::setDT(data.table::copy(data))
  dt1 <- dt1[, c(iv_1_name, iv_2_name, dv_name), with = FALSE]
  names(dt1) <- c("iv1", "iv2", "dv")
  # omit na values
  dt1 <- stats::na.omit(dt1)
  # deal with missing values
  if (is.null(iv_1_levels)) {
    iv_1_levels <- kim::su(dt1[["iv1"]], na.last = NA)
  }
  if (is.null(iv_2_levels)) {
    iv_2_levels <- kim::su(dt1[["iv2"]], na.last = NA)
  }
  # levels inputs must match the unique values
  if (setequal(iv_1_levels, kim::su(dt1[["iv1"]], na.last = NA)) == FALSE) {
    stop(paste0(
      "The set of values given for the 'iv_1_levels' argument do not match ",
      "the set of (non-NA) unique values of iv_1."))
  }
  if (setequal(iv_2_levels, kim::su(dt1[["iv2"]], na.last = NA)) == FALSE) {
    stop(paste0(
      "The set of values given for the 'iv_2_levels' argument do not match ",
      "the set of (non-NA) unique values of iv_2."))
  }
  # convert ivs to factors
  dt1[, iv1 := factor(iv1, levels = iv_1_levels)]
  dt1[, iv2 := factor(iv2, levels = iv_2_levels)]
  # add the factor for simple effects analysis
  iv_1_num_of_levels <- length(iv_1_levels)
  iv_2_num_of_levels <- length(iv_2_levels)
  simple_effect_levels <- paste0(
    rep(iv_1_levels, each = iv_2_num_of_levels), "_",
    rep(iv_2_levels, iv_1_num_of_levels))
  dt1[, "simple" := factor(
    paste0(iv1, "_", iv2), levels = simple_effect_levels)]
  # contrast table
  contrast_table_ncol <- iv_1_num_of_levels * iv_2_num_of_levels
  contrast_table_nrow <- contrast_table_ncol - 1
  # contrasts, 1st stage
  num_of_contrasts_for_iv_1 <- iv_1_num_of_levels - 1
  temp_1 <- lapply(
    seq_len(num_of_contrasts_for_iv_1), function(i) {
      focal_levels_1 <- iv_1_levels[i]
      focal_levels_2 <- iv_1_levels[(i + 1):iv_1_num_of_levels]
      focal_levels_all <- c(focal_levels_1, focal_levels_2)
      weights_section_1 <- rep(0, length(
        setdiff(iv_1_levels, focal_levels_all)) * iv_2_num_of_levels)
      weights_section_3_n <- length(
        rep(focal_levels_2, each = iv_2_num_of_levels))
      # weights for weights_section_3 will always be 1
      weights_section_3 <- rep(1, weights_section_3_n)
      weights_section_2 <- rep(
        -sum(weights_section_3) / iv_2_num_of_levels, iv_2_num_of_levels)
      contrast <- c(weights_section_1, weights_section_2, weights_section_3)
      return(contrast)
    })
  contrasts_for_iv_1 <- do.call(rbind, temp_1)
  # contrasts, 2nd stage
  num_of_contrasts_for_iv_2 <- iv_2_num_of_levels - 1
  temp_2 <- lapply(
    seq_len(iv_1_num_of_levels), function(i) {
      temp_3 <- lapply(
        seq_len(num_of_contrasts_for_iv_2), function(j) {
          focal_levels_1 <- iv_2_levels[j]
          focal_levels_2 <- iv_2_levels[(j + 1):iv_2_num_of_levels]
          focal_levels_all <- c(focal_levels_1, focal_levels_2)
          weights_section_1 <- rep(0, (i - 1) * iv_2_num_of_levels + length(
            setdiff(iv_2_levels, focal_levels_all)))
          weights_section_3_n <- length(focal_levels_2)
          # weights for weights_section_3 will always be 1
          weights_section_3 <- rep(1, weights_section_3_n)
          weights_section_2 <- rep(
            -sum(weights_section_3) / length(focal_levels_1),
            length(focal_levels_1))
          weights_section_4 <- rep(0, contrast_table_ncol - length(
            c(weights_section_1, weights_section_2, weights_section_3)))
          contrast <- c(weights_section_1, weights_section_2,
                        weights_section_3, weights_section_4)
          return(contrast)
        })
      output <- do.call(rbind, temp_3)
      return(output)
    })
  contrasts_for_iv_2 <- do.call(rbind, temp_2)
  # construct the contrast table
  contrast_dt <- data.table::data.table(
    rbind(contrasts_for_iv_1, contrasts_for_iv_2))
  names(contrast_dt) <- simple_effect_levels
  # contrast names
  contrast_names <- c(
    paste0(iv_1_name, " ", seq_len(iv_1_num_of_levels - 1)),
    paste0(iv_2_name, " at ", iv_1_name, " = ", iv_1_levels))
  # check if orthogonal
  contrast_dt_2 <- data.table::copy(contrast_dt)
  contrast_dt_2[, "weight_sum" := rowSums(contrast_dt_2)]
  if (setequal(0, contrast_dt_2[["weight_sum"]]) == FALSE) {
    stop(paste0(
      "The sums of contrast weights are not all equal to 0.",
      " The set of contrasts may not be orthogoal."))
  }
  names(contrast_dt_2)[max(which(names(
    contrast_dt_2) == "weight_sum"))] <- "..Sum of Weights"
  group_weights_products <- data.table::setDT(lapply(contrast_dt_2, prod))
  if (sum(group_weights_products) != 0) {
    stop(paste0(
      "The products of group weights do not add up to 0.",
      " The set of contrasts may not be orthogoal."))
  }
  contrast_dt_2 <- rbind(contrast_dt_2, group_weights_products)
  contrast_dt_2[, "Contrast" := c(
    contrast_names, "..Product of Group Weights")]
  data.table::setcolorder(contrast_dt_2, "Contrast")
  # print depending on conditions
  if (print_contrast_table == "weights_only") {
    print(contrast_dt)
  } else if (print_contrast_table == "weights_sums_and_products") {
    print(contrast_dt_2)
  }
  # contrasts as a matrix for anova
  contrast_mat <- t(as.matrix(contrast_dt))
  colnames(contrast_mat) <- paste0(": ", contrast_names)
  stats::contrasts(dt1$simple) <- contrast_mat
  # simple effects analysis
  simple_effects_formula <- stats::as.formula("dv ~ simple")
  simple_effects_model <- stats::aov(simple_effects_formula, data = dt1)
  # print the results
  print(stats::summary.lm(simple_effects_model))
  # output of the function
  if (is.null(output) == FALSE) {
    if (output == "lm_object") {
      output <- simple_effects_model
    } else if (output == "table") {
      output <- stats::summary.lm(simple_effects_model)$coefficients
    } else if (output == "weights_only") {
      output <- contrast_dt
    } else if (output == "weights_sums_and_products") {
      output <- contrast_dt_2
    } else if (output == "all") {
      output <- list(
        lm_object = simple_effects_model,
        table = stats::summary.lm(simple_effects_model)$coefficients,
        weights_only = contrast_dt,
        weights_sums_and_products = contrast_dt_2)
    }
    invisible(output)
  }
}
