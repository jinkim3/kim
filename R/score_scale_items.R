#' Score scale items
#'
#' Score items in a scale (e.g., Likert scale items) by computing the
#' sum or mean of the items.
#'
#' @param items list of scale items (i.e., list of vectors of ratings)
#' to code normally (as opposed to reverse coding).
#' @param reverse list of scale items to reverse code.
#' @param operation if \code{operation = "mean"}, mean of the scale items
#' will be calculated; if \code{operation = "sum"}, sum of the scale items
#' will be calculated (default = "mean").
#' @param na_summary logical. If \code{na_summary = TRUE} a summary of
#' NA values will be printed; if \code{na_summary = FALSE} the summary
#' will not be printed (default = TRUE).
#' @param reverse_code_minuend required for reverse coding; the number
#' from which to subtract item ratings when reverse-coding. For example,
#' if the items to reverse code are measured on a 7-point scale, enter
#' \code{reverse_code_minuend = 8}.
#' @examples
#' score_scale_items(items = list(1:5, rep(3, 5)),
#' reverse = list(rep(5, 5)), reverse_code_minuend = 6)
#' score_scale_items(items = list(c(1, 1), c(1, 5)),
#' reverse = list(c(5, 3)), reverse_code_minuend = 6, na_summary = FALSE)
#' score_scale_items(items = list(c(1, 1), c(1, 5)),
#' reverse = list(c(5, 1)), reverse_code_minuend = 6, operation = "sum")
#' @export
#' @import data.table
score_scale_items <- function(
  items = NULL,
  reverse = NULL,
  operation = "mean",
  na_summary = TRUE,
  reverse_code_minuend = NULL) {
  # deal with argument inputs
  if (!is.null(reverse)) {
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
    if (!is.null(items)) {
      na_summary_1 <- vapply(seq_along(items), function(i) {
        sum(is.na(items[[i]]))
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
    if (!is.null(reverse)) {
      na_summary_2 <- vapply(seq_along(reverse), function(i) {
        sum(is.na(reverse[[i]]))
      }, FUN.VALUE = numeric(1L))
      if (length(na_summary_2) > 0) {
        na_summary_2_name <- paste0(
          "reverse_coded_item_", seq_along(na_summary_2))
      } else {
        na_summary_2_name <- NULL
      }
    } else {
      na_summary_2_name <- NULL
    }
    # na summary table
    na_summary_final <- data.table(
      item = c(na_summary_1_name, na_summary_2_name),
      number_of_na_values = c(na_summary_1, na_summary_2))
    # report
    if (any(na_summary_final[["number_of_na_values"]] > 0)) {
      message("The NA values will be omitted when scoring scale items.")
    } else {
      message("There were no NA values in any of the items.")
    }
    print(na_summary_final)
  }
  # reverse code items
  if (!is.null(reverse)) {
    reverse_coded <- lapply(seq_along(reverse), function(i) {
      ratings <- reverse[[i]]
      output <- reverse_code_minuend - ratings
      return(output)
    })
    items_after_rev_coding <- c(items, reverse_coded)
  } else {
    items_after_rev_coding <- items
  }
  # data table of item ratings
  ratings_dt <- setDT(items_after_rev_coding)[]
  # calculate mean
  if (operation == "mean") {
    output <- rowMeans(ratings_dt, na.rm = TRUE)
  }
  # calculate sum
  if (operation == "sum") {
    output <- rowSums(ratings_dt, na.rm = TRUE)
  }
  # final output
  return(output)
}
