#' Clean data from Qualtrics
#'
#' Clean a data set downloaded from Qualtrics
#'
#' @param data a data object (a data frame or a data.table)
#' @param default_cols_by_qualtrics names of columns that Qualtrics
#' includes in the data set by default (e.g., "StartDate", "Finished").
#' Accepting the default value \code{default_cols_by_qualtrics = NULL}
#' will set the names to be those that Qualtrics uses as of Dec 25, 2020.
#' @param default_cols_by_qualtrics_new new names for columns that
#' Qualtrics includes in the data set by default
#' (e.g., "StartDate", "Finished").
#' Accepting the default value \code{default_cols_by_qualtrics_new = NULL}
#' will set the names to be those that Qualtrics uses as of Dec 25, 2020
#' converted to snake_case (e.g., "start_date", "finished").
#' @param warn_accuracy_loss logical. whether to warn the user if
#' converting character to numeric leads to loss of accuracy.
#' (default = FALSE)
#' @param click_data_cols if \code{click_data_cols = "rm"},
#' columns containing click data (e.g., "_First Click") will be
#' removed. If \code{click_data_cols = "move_to_right"}, the columns
#' will be moved to the right (end) of the data set.
#' @param page_submit_cols if \code{page_submit_cols = "rm"},
#' columns containing page submit data (e.g., "_Page Submit";
#' "response time" data) will be removed.
#' If \code{page_submit_cols = "move_to_right"}, the columns
#' will be moved to the right (end) of the data set.
#' @return a data.table object
#' @examples
#' clean_data_from_qualtrics(mtcars)
#' clean_data_from_qualtrics(mtcars, default_cols_by_qualtrics = "mpg",
#' default_cols_by_qualtrics_new = "mpg2")
#' @import data.table
#' @export
clean_data_from_qualtrics <- function(
  data = NULL,
  default_cols_by_qualtrics = NULL,
  default_cols_by_qualtrics_new = NULL,
  warn_accuracy_loss = FALSE,
  click_data_cols = "rm",
  page_submit_cols = "move_to_right"
) {
  # bind the vars locally to the function
  qualt_start_date <- qualt_end_date <- NULL
  # convert data to data.table
  dt <- setDT(copy(data))
  # change id
  change_id <- 0
  # change column names according to the argument inputs
  if (is.null(default_cols_by_qualtrics) &
      is.null(default_cols_by_qualtrics_new)) {
    # use qualtrics's default column names as of dec 25, 2020
    default_cols_by_qualtrics <- c(
      "StartDate", "EndDate", "Status", "IPAddress", "Progress",
      "Duration (in seconds)", "Finished", "RecordedDate",
      "ResponseId", "RecipientLastName", "RecipientFirstName",
      "RecipientEmail", "ExternalReference", "LocationLatitude",
      "LocationLongitude", "DistributionChannel", "UserLanguage")
    # new names in snake case
    default_cols_by_qualtrics_new <- c(
      "qualt_start_date", "qualt_end_date",
      "qualt_status", "qualt_ip_address",
      "qualt_progress", "qualt_duration_in_seconds",
      "qualt_finished", "qualt_recorded_date",
      "qualt_response_id", "qualt_recipient_last_name",
      "qualt_recipient_first_name",
      "qualt_recipient_email", "qualt_external_ref",
      "qualt_location_latitude", "qualt_location_longitude",
      "qualt_dist_channel", "qualt_user_language")
  }
  # check if the length matches for old and new column name vectors
  if (length(default_cols_by_qualtrics) !=
      length(default_cols_by_qualtrics_new)) {
    stop(paste0(
      "The number of default column names by Qualtrics (",
      length(default_cols_by_qualtrics),
      ") does not match that of new column names (",
      length(default_cols_by_qualtrics_new), ")."))
  }
  # check if the column names set as default_cols_by_qualtrics
  # exist in the data set
  if (length(intersect(default_cols_by_qualtrics, names(dt))) > 0) {
    # indices of default qualtrics column names
    col_number <- match(default_cols_by_qualtrics, names(dt))
    # change default column names
    setnames(
      x = dt,
      old = default_cols_by_qualtrics,
      new = default_cols_by_qualtrics_new,
      skip_absent = TRUE)
    # update change id
    change_id <- change_id + 1
    # report
    message(paste0(
      "\nFollowing changes were made to the data set:\n\n",
      change_id,
      ". Column names were changed as follows:\n"))
    # summarize column name changes
    col_name_change_summary <- data.table(
      col_number,
      old_col_name = default_cols_by_qualtrics,
      new_col_name = default_cols_by_qualtrics_new)
    print(col_name_change_summary)
  }
  # check if first two rows should be removed
  if ("qualt_start_date" %in% names(dt) &
      "qualt_end_date" %in% names(dt)) {
    if (grep("ImportId", dt[, qualt_start_date]) == 2 &
        grep("ImportId", dt[, qualt_end_date]) == 2) {
      dt <- dt[-(1:2), ]
      # update change id
      change_id <- change_id + 1
      # report
      message(paste0(
        "\n",
        change_id,
        ". The first two rows that look like column headers",
        " were deleted.\n"))
    }
  }
  # warn accuracy loss when converting numbers?
  type_convert_numerals_arg <- ifelse(
    warn_accuracy_loss == TRUE, "warn.loss", "allow.loss")
  # convert to numeric
  stopifnot(is.list(dt))
  number_of_all_cols <- length(dt)
  number_of_char_cols_before <- sum(
    vapply(dt, class, FUN.VALUE = character(1L)) == "character")
  dt[] <- rapply(
    dt, utils::type.convert, classes = "character",
    how = "replace", as.is = TRUE,
    numerals = type_convert_numerals_arg)
  number_of_char_cols_after <- sum(
    vapply(dt, class, FUN.VALUE = character(1L)) == "character")
  number_of_converted_cols <-
    number_of_char_cols_before - number_of_char_cols_after
  if (number_of_converted_cols > 0) {
    # update change id
    change_id <- change_id + 1
    # report
    message(paste0(
      change_id,
      ". Number of character columns converted to numeric: ",
      number_of_converted_cols,
      " out of ",
      number_of_all_cols,
      ".\n"))
  }
  # check number of columns
  number_of_remaining_cols <- length(dt)
  number_of_page_submit_cols <- length(grep(
    "_Page Submit$", names(dt)))
  if (number_of_page_submit_cols > 0) {
    # remove page submit (response time) data
    if (page_submit_cols == "rm") {
      dt <- dt[, (grep("_Page Submit$", names(dt))) := NULL]
      # update change id
      change_id <- change_id + 1
      # report
      message(paste0(
        change_id,
        ". Number of columns with page submit data that were removed ",
        "from the data set: ",
        number_of_page_submit_cols,
        " out of ",
        number_of_remaining_cols,
        ".\n"))
    }
    # move page submit (response time) data columns to the right
    if (page_submit_cols == "move_to_right") {
      number_of_remaining_cols <- length(dt)
      number_of_page_submit_cols <- length(grep(
        "_Page Submit$", names(dt)))
      setcolorder(
        dt,
        names(dt)[grep(
          "_Page Submit$",
          names(dt), invert = TRUE)])
      # update change id
      change_id <- change_id + 1
      # report
      message(paste0(
        change_id,
        ". Number of columns with page submit data that were moved ",
        "to the right: ",
        number_of_page_submit_cols,
        " out of ",
        number_of_remaining_cols,
        ".\n"))
    }
  }
  # check number of columns
  number_of_remaining_cols <- length(dt)
  number_of_click_data_cols <- length(grep(
    "_First Click|_Last Click|_Click Count$", names(dt)))
  # check if click data columns exist
  if (number_of_click_data_cols > 0) {
    # remove click data
    if (click_data_cols == "rm") {
      dt <- dt[, (grep(
        "_First Click|_Last Click|_Click Count$", names(dt))) := NULL]
      # update change id
      change_id <- change_id + 1
      # report
      message(paste0(
        change_id,
        ". Number of columns with click data that were removed ",
        "from the data set: ",
        number_of_click_data_cols,
        " out of ",
        number_of_remaining_cols,
        ".\n"))
    }
    # move click data columns to the end
    if (click_data_cols == "move_to_right") {
      setcolorder(
        dt,
        names(dt)[grep(
          "_First Click|_Last Click|_Click Count$",
          names(dt), invert = TRUE)])
      # update change id
      change_id <- change_id + 1
      # report
      message(paste0(
        change_id,
        ". Number of columns with click data that were moved ",
        "to the right: ",
        number_of_click_data_cols,
        " out of ",
        number_of_remaining_cols,
        ".\n"))
    }
  }
  # indices of meta info column names
  meta_info_col_indices <- grep(
    "_Browser|_Version|_Operating System|_Resolution$", names(dt))
  if (length(meta_info_col_indices) > 0) {
    # if meta info columns are right next to each other,
    # (i.e., the meta info columns most likely set by qualtrics),
    # change the column name suffix to snake case
    if (all(abs(diff(meta_info_col_indices)) == 1)) {
      names(dt)[meta_info_col_indices] <-
        gsub("_Browser$", "_browser",
             names(dt)[meta_info_col_indices])
      names(dt)[meta_info_col_indices] <-
        gsub("_Version$", "_version",
             names(dt)[meta_info_col_indices])
      names(dt)[meta_info_col_indices] <-
        gsub("_Operating System$", "_os",
             names(dt)[meta_info_col_indices])
      names(dt)[meta_info_col_indices] <-
        gsub("_Resolution$", "_resolution",
             names(dt)[meta_info_col_indices])
      # update change id
      change_id <- change_id + 1
      # report
      message(paste0(
        change_id,
        ". Names of meta information columns were ",
        "converted to snake_case.\n"))
    }
  }
  invisible(dt)
}
