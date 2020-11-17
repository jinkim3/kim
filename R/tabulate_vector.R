#' Tabulate vector
#'
#' Shows frequency and proportion of unique values in a table format
#'
#' @param vector a character or numeric vector
#' @param na.rm if \code{TRUE}, NA values will be removed before calculating
#' frequencies and proportions.
#' @param sort_by_decreasing_count if \code{TRUE}, the output table will
#' be sorted in the order of decreasing frequency.
#' @param sort_by_increasing_count if \code{TRUE}, the output table will
#' be sorted in the order of increasing frequency.
#' @param sort_by_decreasing_value if \code{TRUE}, the output table will
#' be sorted in the order of decreasing value.
#' @param sort_by_increasing_value if \code{TRUE}, the output table will
#' be sorted in the order of increasing value.
#' @param total_included if \code{TRUE}, the output table will include
#' a row for total counts.
#' @param sigfigs number of significant digits to round to
#' @param round_to_nth_digit_after_decimal round to nth digit after decimal
#' (alternative to \code{sigfigs})
#' @param output_type if \code{output_type = "df"}, return a data.frame.
#' By default, \code{output_type = "dt"}, which will return a data.table.
#' @return a data.table or data.frame
#' @examples
#' tabulate_vector(c("a", "b", "b", "c", "c", "c", NA))
#' tabulate_vector(c("a", "b", "b", "c", "c", "c", NA),
#' sort_by_increasing_count = TRUE)
#' tabulate_vector(c("a", "b", "b", "c", "c", "c", NA),
#' sort_by_decreasing_value = TRUE)
#' tabulate_vector(c("a", "b", "b", "c", "c", "c", NA),
#' sort_by_increasing_value = TRUE)
#' tabulate_vector(c("a", "b", "b", "c", "c", "c", NA),
#' sigfigs = 4)
#' tabulate_vector(c("a", "b", "b", "c", "c", "c", NA),
#' round_to_nth_digit_after_decimal = 1)
#' tabulate_vector(c("a", "b", "b", "c", "c", "c", NA),
#' output_type = "df")
#' @export
# tabulate vector frequency table
tabulate_vector <-
  function(
    vector = NULL,
    na.rm = TRUE,
    sort_by_decreasing_count = NULL,
    sort_by_increasing_count = NULL,
    sort_by_decreasing_value = NULL,
    sort_by_increasing_value = NULL,
    total_included = TRUE,
    sigfigs = NULL,
    round_to_nth_digit_after_decimal = NULL,
    output_type = "dt") {
    # deal with NA values
    if(na.rm == TRUE) {
      temp_1 <- vector[!is.na(vector)]
    } else if(na.rm == FALSE) {
      temp_1 <- vector
    } else {
      stop("Unrecognized value for the argument, na.rm")
    }
    # unique values
    value <- sort(unique(temp_1), na.last = TRUE)
    # count
    count <- vapply(value, function(x) {
      if(is.na(x)) {
        count_for_given_value <- sum(is.na(temp_1))
      } else {
        count_for_given_value <- sum(sum(temp_1 == x, na.rm = TRUE))
      }
      return(count_for_given_value)
    }, FUN.VALUE = numeric(1))
    # total count
    total_count <- sum(count)
    # percent
    percent <- vapply(value, function(x) {
      if(is.na(x)) {
        percent_for_given_value <-
          sum(is.na(temp_1)) / total_count * 100
      } else {
        percent_for_given_value <-
          sum(temp_1 == x, na.rm = TRUE) / total_count * 100
      }
      return(percent_for_given_value)
    }, FUN.VALUE = numeric(1))
    # data table without totals
    dt_1 <- data.table::data.table(
      value, count, percent)
    # set the default sorting method
    if(sum(c(
      is.null(sort_by_decreasing_count),
      is.null(sort_by_increasing_count),
      is.null(sort_by_decreasing_value),
      is.null(sort_by_increasing_value))) == 4) {
      sort_by_decreasing_count <- TRUE
    }
    # check the argument inputs for sorting
    unique_values_in_sort_args <- unique(c(
      sort_by_decreasing_count,
      sort_by_increasing_count,
      sort_by_decreasing_value,
      sort_by_increasing_value))
    # sort based on argument inputs
    if(identical(unique_values_in_sort_args, TRUE)) {
      if(sum(c(
        sort_by_decreasing_count,
        sort_by_increasing_count,
        sort_by_decreasing_value,
        sort_by_increasing_value)) == 1) {
        if(!is.null(sort_by_decreasing_count)) {
          data.table::setorder(dt_1, -count)
        } else if(!is.null(sort_by_increasing_count)) {
          data.table::setorder(dt_1, count)
        } else if(!is.null(sort_by_decreasing_value)) {
          data.table::setorder(dt_1, -value)
        } else if(!is.null(sort_by_increasing_value)) {
          data.table::setorder(dt_1, value)
        }
      } else {
        stop(paste0(
          "Please make sure that only 1 of the 4 arguments for sorting",
          " is set as TRUE and that 3 other arguments take the ",
          "default value of NULL."))
      }
    } else {
      stop(paste0(
        "Please make sure that only 1 of the 4 arguments for sorting",
        " is set as TRUE and that 3 other arguments take the ",
        "default value of NULL."))
    }
    # include totals
    if(total_included == TRUE) {
      dt_2 <- data.table::data.table(
        value = "..Total:",
        count = total_count,
        percent = sum(dt_1[["percent"]]))
      dt_1 <- data.table::rbindlist(list(dt_1, dt_2))
    }
    # set the default rounding method
    if(sum(c(
      is.null(sigfigs),
      is.null(round_to_nth_digit_after_decimal))) == 2) {
      sigfigs <- 2
    }
    # round percentages
    if(is.numeric(sigfigs)) {
      dt_1[["percent"]] <- signif(dt_1[["percent"]], sigfigs)
      if(is.numeric(round_to_nth_digit_after_decimal)) {
        message(paste0(
          "Only the sigfigs argument was used.\n",
          "Your input for round_to_nth_digit_after_decimal ",
          "argument was ignored."))
      }
    } else if(is.numeric(round_to_nth_digit_after_decimal)) {
      dt_1[["percent"]] <- round(
        dt_1[["percent"]], round_to_nth_digit_after_decimal)
    }
    # set output_type
    if(output_type == "df") {
      dt_1 <- as.data.frame(dt_1)
    }
    return(dt_1)
  }
