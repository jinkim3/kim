#' Descriptive statistics by group
#'
#' Returns descriptive statistics by group
#'
#' @param data a data object (a data frame or a data.table)
#' @param var_for_stats name of the variable for which descriptive
#' statistics will be calculated
#' @param grouping_vars name(s) of grouping variables
#' @param sigfigs number of significant digits to round to
#' @param cols_to_round names of columns whose values will be rounded
#' @return the output will be a data.table showing descriptive statistics
#' of the variable for each of the groups formed by the grouping variables.
#' @examples
#' desc_stats_by_group(data = mtcars, var_for_stats = "mpg",
#' grouping_vars = c("vs", "am"))
#' desc_stats_by_group(data = mtcars, var_for_stats = "mpg",
#' grouping_vars = c("vs", "am"), sigfigs = 3)
#' desc_stats_by_group(data = mtcars, var_for_stats = "mpg",
#' grouping_vars = c("vs", "am"), stats = "basic", sigfigs = 2)
#' desc_stats_by_group(data = mtcars, var_for_stats = "mpg",
#' grouping_vars = c("vs", "am"), stats = "basic", sigfigs = 2,
#' cols_to_round = "all")
#' @export
#' @import data.table
desc_stats_by_group <- function(
  data = NULL,
  var_for_stats = NULL,
  grouping_vars = NULL,
  stats = "all",
  sigfigs = NULL,
  cols_to_round = NULL) {
  # copy data
  dt1 <- data.table::setDT(data.table::copy(data))
  # omit na values
  dt2 <- dt1[!is.na(get(var_for_stats))]
  # starting dt
  dt3 <- dt2[, list(.temp_col. = 1),
             keyby = grouping_vars][, .temp_col. := NULL][]
  # which stats to include?
  if (stats == "all") {
    dt4 <- dt2[, list(
      n = length(get(var_for_stats)),
      mean = as.numeric(mean(get(var_for_stats), na.rm = TRUE)),
      sd = as.numeric(stats::sd(get(var_for_stats), na.rm = TRUE)),
      median = as.numeric(stats::median(get(var_for_stats), na.rm = TRUE)),
      min = as.numeric(min(get(var_for_stats), na.rm = TRUE)),
      max = as.numeric(max(get(var_for_stats), na.rm = TRUE)),
      se = as.numeric(kim::se_of_mean(get(var_for_stats), na.rm = TRUE)),
      ci_95_ll = tryCatch(
        as.numeric(stats::t.test(get(var_for_stats))[["conf.int"]][1]),
        warning = function(w) NA_real_, error = function(e) NA_real_),
      ci_95_ul = tryCatch(
        as.numeric(stats::t.test(get(var_for_stats))[["conf.int"]][2]),
        warning = function(w) NA_real_, error = function(e) NA_real_),
      pi_95_ll = tryCatch(
        as.numeric(
          mean(get(var_for_stats), na.rm = TRUE) +
            stats::sd(get(var_for_stats), na.rm = TRUE) *
            stats::qt(0.025, length(get(var_for_stats)) - 1)),
        warning = function(w) NA_real_, error = function(e) NA_real_),
      pi_95_ul = tryCatch(
        as.numeric(
          mean(get(var_for_stats), na.rm = TRUE) +
            stats::sd(get(var_for_stats), na.rm = TRUE) *
            stats::qt(0.975, length(get(var_for_stats)) - 1)),
        warning = function(w) NA_real_, error = function(e) NA_real_),
      skewness = as.numeric(kim::skewness(get(var_for_stats))),
      kurtosis = as.numeric(kim::kurtosis(get(var_for_stats)))),
      keyby = grouping_vars]
    # keep only the stats
    dt4 <- dt4[, (seq_along(grouping_vars)) := NULL][]
  } else if (stats == "basic") {
    dt4 <- dt2[, list(
      n = length(get(var_for_stats)),
      mean = as.numeric(mean(get(var_for_stats), na.rm = TRUE)),
      sd = as.numeric(stats::sd(get(var_for_stats), na.rm = TRUE)),
      median = as.numeric(stats::median(get(var_for_stats), na.rm = TRUE)),
      min = as.numeric(min(get(var_for_stats), na.rm = TRUE)),
      max = as.numeric(max(get(var_for_stats), na.rm = TRUE))),
      keyby = grouping_vars]
    # keep only the stats
    dt4 <- dt4[, (seq_along(grouping_vars)) := NULL][]
  } else {
    if ("n" %in% stats) {
      dt4[, "n" := dt2[, list(
        .temp_col. = length(get(var_for_stats))),
        keyby = grouping_vars][, .temp_col.]]
    }
    if ("mean" %in% stats) {
      dt4[, "mean" := dt2[, list(
        .temp_col. = as.numeric(mean(get(var_for_stats), na.rm = TRUE))),
        keyby = grouping_vars][, .temp_col.]]
    }
    if ("sd" %in% stats) {
      dt4[, "sd" := dt2[, list(
        .temp_col. = as.numeric(
          stats::sd(get(var_for_stats), na.rm = TRUE))),
        keyby = grouping_vars][, .temp_col.]]
    }
    if ("median" %in% stats) {
      dt4[, "median" := dt2[, list(
        .temp_col. = as.numeric(
          stats::median(get(var_for_stats), na.rm = TRUE))),
        keyby = grouping_vars][, .temp_col.]]
    }
    if ("min" %in% stats) {
      dt4[, "min" := dt2[, list(
        .temp_col. = as.numeric(min(get(var_for_stats), na.rm = TRUE))),
        keyby = grouping_vars][, .temp_col.]]
    }
    if ("max" %in% stats) {
      dt4[, "max" := dt2[, list(
        .temp_col. = as.numeric(max(get(var_for_stats), na.rm = TRUE))),
        keyby = grouping_vars][, .temp_col.]]
    }
    if ("se" %in% stats) {
      dt4[, "se" := dt2[, list(
        .temp_col. = as.numeric(
          kim::se_of_mean(get(var_for_stats), na.rm = TRUE))),
        keyby = grouping_vars][, .temp_col.]]
    }
    if ("ci_95_ll" %in% stats) {
      dt4[, "ci_95_ll" := dt2[, list(
        .temp_col. = tryCatch(
          as.numeric(stats::t.test(get(var_for_stats))[["conf.int"]][1]),
          warning = function(w) NA_real_, error = function(e) NA_real_)),
        keyby = grouping_vars][, .temp_col.]]
    }
    if ("ci_95_ul" %in% stats) {
      dt4[, "ci_95_ul" := dt2[, list(
        .temp_col. = tryCatch(
          as.numeric(stats::t.test(get(var_for_stats))[["conf.int"]][2]),
          warning = function(w) NA_real_, error = function(e) NA_real_)),
        keyby = grouping_vars][, .temp_col.]]
    }
    if ("pi_95_ll" %in% stats) {
      dt4[, "pi_95_ll" := dt2[, list(
        .temp_col. = tryCatch(
          as.numeric(
            mean(get(var_for_stats), na.rm = TRUE) +
              stats::sd(get(var_for_stats), na.rm = TRUE) *
              stats::qt(0.025, length(get(var_for_stats)) - 1)),
          warning = function(w) NA_real_, error = function(e) NA_real_)),
        keyby = grouping_vars][, .temp_col.]]
    }
    if ("pi_95_ul" %in% stats) {
      dt4[, "pi_95_ul" := dt2[, list(
        .temp_col. = tryCatch(
          as.numeric(
            mean(get(var_for_stats), na.rm = TRUE) +
              stats::sd(get(var_for_stats), na.rm = TRUE) *
              stats::qt(0.975, length(get(var_for_stats)) - 1)),
          warning = function(w) NA_real_, error = function(e) NA_real_)),
        keyby = grouping_vars][, .temp_col.]]
    }
    if ("skewness" %in% stats) {
      dt4[, "skewness" := dt2[, list(
        .temp_col. = as.numeric(kim::skewness(get(var_for_stats)))),
        keyby = grouping_vars][, .temp_col.]]
    }
    if ("kurtosis" %in% stats) {
      dt4[, "kurtosis" := dt2[, list(
        .temp_col. = as.numeric(kim::kurtosis(get(var_for_stats)))),
        keyby = grouping_vars][, .temp_col.]]
    }
  }
  # round to significant digits
  if (!is.null(sigfigs)) {
    if (is.null(cols_to_round)) {
      # set defaults
      if (stats == "all") {
        cols_to_round <- c(
          "mean", "sd", "median", "se", "ci_95_ll", "ci_95_ul", "pi_95_ll",
          "pi_95_ul", "skewness", "kurtosis")
      } else if (stats == "basic") {
        cols_to_round <- c("mean", "sd", "median")
      } else {
        cols_to_round <- intersect(
          c("mean", "sd", "median", "se", "ci_95_ll", "ci_95_ul",
            "pi_95_ll", "pi_95_ul", "skewness", "kurtosis"), stats)
      }
    } else if (cols_to_round == "all") {
      cols_to_round <- intersect(c(
        "mean", "sd", "median", "min", "max", "se", "ci_95_ll",
        "ci_95_ul", "pi_95_ll", "pi_95_ul", "skewness", "kurtosis"),
        names(dt4))
    }
    dt4 <- dt4[, (cols_to_round) := kim::round_flexibly(.SD, sigfigs),
               .SDcols = cols_to_round][]
  }
  # merge the two dt
  dt5 <- cbind(dt3, dt4)
  return(dt5)
}
