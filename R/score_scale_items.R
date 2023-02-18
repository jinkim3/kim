#' Score scale items
#'
#' Score items in a scale (e.g., Likert scale items) by computing the
#' sum or mean of the items.
#'
#' @param item_list a list of scale items (i.e., list of vectors of ratings)
#' to code normally (as opposed to reverse coding).
#' @param reverse_item_list a list of scale items to reverse code.
#' @param operation if \code{operation = "mean"}, mean of the scale items
#' will be calculated; if \code{operation = "sum"}, sum of the scale items
#' will be calculated (default = "mean").
#' @param na.rm logical. The `na.rm` argument that will be passed
#' onto the base R's `rowMeans` or `rowSums` function (default = FALSE).
#' @param na_summary logical. If \code{na_summary = TRUE} a summary of
#' NA values will be printed; if \code{na_summary = FALSE} the summary
#' will not be printed (default = TRUE).
#' @param reverse_code_minuend required for reverse coding; the number
#' from which to subtract item ratings when reverse-coding. For example,
#' if the items to reverse code are measured on a 7-point scale, enter
#' \code{reverse_code_minuend = 8}.
#' @examples
#' score_scale_items(item_list = list(1:5, rep(3, 5)),
#' reverse_item_list = list(rep(5, 5)), reverse_code_minuend = 6)
#' score_scale_items(item_list = list(c(1, 1), c(1, 5)),
#' reverse_item_list = list(c(5, 3)),
#' reverse_code_minuend = 6, na_summary = FALSE)
#' score_scale_items(item_list = list(c(1, 1), c(1, 5)),
#' reverse_item_list = list(c(5, 1)),
#' reverse_code_minuend = 6, operation = "sum")
#' score_scale_items(item_list = list(1:5, rep(3, 5)))
#' score_scale_items(item_list = list(c(1, NA, 3), c(NA, 2, 3)))
#' score_scale_items(item_list = list(c(1, NA, 3), c(NA, 2, 3)), na.rm = TRUE)
#' @export
#' @import data.table
score_scale_items <- function(
  item_list = NULL,
  reverse_item_list = NULL,
  operation = "mean",
  na.rm = FALSE,
  na_summary = TRUE,
  reverse_code_minuend = NULL) {
  # deal with argument inputs
  if (!is.null(reverse_item_list)) {
    if (is.null(reverse_code_minuend)) {
      stop(paste0(
        "Please enter the minuend for reverse coding.\n",
        "For example, if you are reverse coding items on a 7-point ",
        'scale,\nenter "reverse_code_minuend = 8" into the function.'))
    }
  }
  # print a summary of na values
  if (na_summary == TRUE) {
    # report na values in normally coded items
    if (!is.null(item_list)) {
      na_summary_1 <- vapply(seq_along(item_list), function(i) {
        sum(is.na(item_list[[i]]))
      }, FUN.VALUE = numeric(1L))
      if (length(na_summary_1) > 0) {
        na_summary_1_name <- paste0("item_", seq_along(na_summary_1))
      } else {
        na_summary_1_name <- NULL
      }
    } else {
      na_summary_1_name <- NULL
    }
    # report na values in reverse coded items
    if (!is.null(reverse_item_list)) {
      na_summary_2 <- vapply(seq_along(reverse_item_list), function(i) {
        sum(is.na(reverse_item_list[[i]]))
      }, FUN.VALUE = numeric(1L))
      if (length(na_summary_2) > 0) {
        na_summary_2_name <- paste0(
          "reverse_coded_item_", seq_along(na_summary_2))
      } else {
        na_summary_2_name <- NULL
      }
    } else {
      na_summary_2_name <- NULL
      na_summary_2 <- NULL
    }
    # na summary table
    na_summary_final <- data.table::data.table(
      item = c(na_summary_1_name, na_summary_2_name),
      number_of_na_values = c(na_summary_1, na_summary_2))
    # report
    if (any(na_summary_final[["number_of_na_values"]] > 0)) {
      message("NA values were observed as follows:")
      print(na_summary_final)
    } else {
      message("Good! There were no NA values in any of the items.")
    }
  }
  # reverse code items
  if (!is.null(reverse_item_list)) {
    reverse_coded <- lapply(seq_along(reverse_item_list), function(i) {
      ratings <- reverse_item_list[[i]]
      output <- reverse_code_minuend - ratings
      return(output)
    })
    items_after_rev_coding <- c(item_list, reverse_coded)
  } else {
    items_after_rev_coding <- item_list
  }
  # data table of item ratings
  ratings_dt <- data.table::setDT(items_after_rev_coding)[]
  # calculate mean
  if (operation == "mean") {
    output <- rowMeans(ratings_dt, na.rm = na.rm)
  }
  # calculate sum
  if (operation == "sum") {
    output <- rowSums(ratings_dt, na.rm = na.rm)
  }
  # final output
  return(output)
}
