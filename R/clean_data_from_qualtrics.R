#' Clean data from Qualtrics
#'
#' Clean a data set downloaded from Qualtrics
#'
#' @param data a data object (a data frame or a data.table)
#' @param remove_survey_preview_data logical. Whether to remove data
#' from survey preview (default = TRUE)
#' @param remove_test_response_data logical. Whether to remove data
#' from test response (default = TRUE)
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
  remove_survey_preview_data = TRUE,
  remove_test_response_data = TRUE,
  default_cols_by_qualtrics = NULL,
  default_cols_by_qualtrics_new = NULL,
  warn_accuracy_loss = FALSE,
  click_data_cols = "rm",
  page_submit_cols = "move_to_right"
) {
  # bind the vars locally to the function
  qualt_dist_channel <- NULL
  # convert data to data.table
  dt <- data.table::setDT(data.table::copy(data))
  # change id
  change_id <- 0
  # determine which format the qualtrics data are in
  if (tryCatch(all(
    ifelse(identical(names(dt)[1:4], paste0("V", 1:4)), TRUE, FALSE),
    ifelse(identical(dt[1:2, get("V1")], c("V1", "ResponseID")),
           TRUE, FALSE),
    ifelse(identical(dt[1:2, get("V2")], c("V2", "ResponseSet")),
           TRUE, FALSE),
    ifelse(identical(dt[1:2, get("V3")], c("V3", "Name")),
           TRUE, FALSE),
    ifelse(identical(dt[1:2, get("V4")], c("V4", "ExternalDataReference")),
           TRUE, FALSE)),
    error = function(e) FALSE,
    warning = function(w) FALSE) == TRUE) {
    # if the data pass the test above, then it is in format 1
    qualt_data_format <- 1
  } else if (tryCatch(all(
    ifelse(identical(names(dt)[1:4], paste0("V", 1:4)), TRUE, FALSE),
    ifelse(dt[["V1"]][1] == "ResponseID", TRUE, FALSE),
    ifelse(dt[["V2"]][1] == "ResponseSet", TRUE, FALSE),
    ifelse(dt[["V3"]][1] == "Name", TRUE, FALSE),
    ifelse(dt[["V4"]][1] == "ExternalDataReference", TRUE, FALSE)),
    error = function(e) FALSE,
    warning = function(w) FALSE) == TRUE) {
    # if the data pass the test above, then it is in format 2
    qualt_data_format <- 2
  } else {
    # if the data pass the test above, then it is in format 3
    qualt_data_format <- 3
  }
  # first part of the cleaning procedure for data in format 1
  if (qualt_data_format == 1) {
    # report
    message(paste0(
      "Cleaning a data set that is in an older Qualtrics format",
      " (that of around April 2016)..."))
    # how many of the columns at the beginning are those automatically
    # generated by qualtrics? for this format, set it as 10
    num_of_qualt_cols_at_begin <- 10
    # row 2 to col names
    dt_length <- length(dt)
    col_names_qualt_data_format_1 <- c(
      "ResponseID", "ResponseSet", "Name", "ExternalDataReference",
      "EmailAddress", "IPAddress", "Status", "StartDate", "EndDate",
      "Finished", "LocationLatitude", "LocationLongitude")
    if (identical(
      as.character(dt[2, ])[c(
        seq_len(num_of_qualt_cols_at_begin),
        (dt_length - 3):(dt_length - 2))],
      col_names_qualt_data_format_1)) {
      new_colnm_qualt_data_format_1 <- paste0("qualt_", c(
        "response_id", "response_set", "name", "external_data_ref",
        "email_address", "ip_address", "status", "start_date",
        "end_date", "finished", "location_latitude",
        "location_longitude"))
      names(dt)[c(1:10, (dt_length - 3):(dt_length - 2))] <-
        new_colnm_qualt_data_format_1
      # update change id
      change_id <- change_id + 1
      # report
      message(paste0(
        "Following changes were made to the data set.\n\n",
        change_id,
        ". Column names were changed as follows:\n"))
      # summarize column name changes
      col_name_change_summary <- data.table(
        col_number = 1:12,
        old_col_name = col_names_qualt_data_format_1,
        new_col_name = new_colnm_qualt_data_format_1)
      print(col_name_change_summary)
    }
    # delete the last two columns
    names_of_last_2_cols <- utils::tail(names(dt), 2)
    if (identical(utils::tail(as.character(dt[2, ]), 2), c(
      "LocationAccuracy", "NA"))) {
      # columns removed
      cols_removed <- c()
      # delete the penultimate column only if it contains no real info
      if (identical(unique(dt[[names(dt)[(dt_length - 1)]]]),
                    c("LocationAccuracy", "-1"))) {
        dt[[names(dt)[(dt_length - 1)]]] <- NULL
        cols_removed <- c(cols_removed, "LocationAccuracy")
      }
      # delete the last column only if it contains no info
      if (identical(unique(dt[[length(dt)]]), NA)) {
        dt[[length(dt)]] <- NULL
        cols_removed <- c(cols_removed, names_of_last_2_cols[2])
      }
      # update change id
      change_id <- change_id + 1
      # report
      message(paste0(
        "\n",
        change_id,
        ". The following column(s) containing no real information ",
        "were removed: ", paste0(cols_removed, collapse = ", ")))
    }
    # change column names from "V1, V2, ..." to "Q1, Q2, ..."
    names(dt)[(num_of_qualt_cols_at_begin + 1):(dt_length - 4)] <-
      as.character(dt[1, ])[
        (num_of_qualt_cols_at_begin + 1):(dt_length - 4)]
    # delete the first x rows that are probably column headers
    if ("qualt_response_id" %in% names(dt) &
        "qualt_response_set" %in% names(dt)) {
      if (any(grepl("^ResponseID$", dt[["qualt_response_id"]][1:2])) &
          any(grepl("^ResponseSet$", dt[["qualt_response_set"]][1:2]))) {
        # how many rows at the beginning are column headers?
        num_col_header_rows <- which(
          dt[["qualt_response_id"]] == "ResponseID")
        # delete the first x rows
        dt <- dt[-(seq_len(num_col_header_rows)), ]
        # update change id
        change_id <- change_id + 1
        # report
        message(paste0(
          "\n",
          change_id,
          ". The first ", num_col_header_rows,
          " rows that look like column headers",
          " were deleted.\n"))
      }
    }
  }
  # first part of the cleaning procedure for data in format 2
  if (qualt_data_format == 2) {
    # report
    message(paste0(
      "Cleaning a data set that is in an older Qualtrics format",
      " (that of around July 7, 2016)..."))
    # row 1 to col names
    dt_length <- length(dt)
    col_names_qualt_data_format_2 <- c(
      "ResponseID", "ResponseSet", "Name", "ExternalDataReference",
      "EmailAddress", "IPAddress", "Status", "StartDate", "EndDate",
      "Finished", "LocationLatitude", "LocationLongitude")
    if (identical(
      as.character(dt[1, ])[c(1:10, (dt_length - 3):(dt_length - 2))],
      col_names_qualt_data_format_2)) {
      new_colnm_qualt_data_format_2 <- paste0("qualt_", c(
        "response_id", "response_set", "name", "external_data_ref",
        "email_address", "ip_address", "status", "start_date",
        "end_date", "finished", "location_latitude",
        "location_longitude"))
      names(dt)[c(1:10, (dt_length - 3):(dt_length - 2))] <-
        new_colnm_qualt_data_format_2
      # update change id
      change_id <- change_id + 1
      # report
      message(paste0(
        "Following changes were made to the data set.\n",
        change_id,
        ". Column names were changed as follows:\n"))
      # summarize column name changes
      col_name_change_summary <- data.table(
        col_number = 1:12,
        old_col_name = col_names_qualt_data_format_2,
        new_col_name = new_colnm_qualt_data_format_2)
      print(col_name_change_summary)
    }
    # delete the last two columns
    names_of_last_2_cols <- utils::tail(names(dt), 2)
    if (identical(utils::tail(as.character(dt[1, ]), 2), c(
      "LocationAccuracy", "NA"))) {
      # columns removed
      cols_removed <- c()
      # delete the penultimate column only if it contains no real info
      if (identical(unique(dt[[names(dt)[(dt_length - 1)]]]),
                    c("LocationAccuracy", "-1"))) {
        dt[[names(dt)[(dt_length - 1)]]] <- NULL
        cols_removed <- c(cols_removed, "LocationAccuracy")
      }
      # delete the last column only if it contains no info
      if (identical(unique(dt[[length(dt)]]), NA)) {
        dt[[length(dt)]] <- NULL
        cols_removed <- c(cols_removed, names_of_last_2_cols[2])
      }
      # update change id
      change_id <- change_id + 1
      # report
      message(paste0(
        "\n",
        change_id,
        ". The following column(s) containing no real information ",
        "were removed: ", paste0(cols_removed, collapse = ", ")))
    }
    # delete the first x rows that are probably column headers
    if ("qualt_response_id" %in% names(dt) &
        "qualt_response_set" %in% names(dt)) {
      if (any(grepl("^ResponseID$", dt[["qualt_response_id"]][1:2])) &
          any(grepl("^ResponseSet$", dt[["qualt_response_set"]][1:2]))) {
        # how many rows at the beginning are column headers?
        num_col_header_rows <- which(
          dt[["qualt_response_id"]] == "ResponseID")
        # delete the first x rows
        dt <- dt[-(seq_len(num_col_header_rows)), ]
        # update change id
        change_id <- change_id + 1
        # report
        message(paste0(
          "\n",
          change_id,
          ". The first ", num_col_header_rows,
          " rows that look like column headers",
          " were deleted.\n"))
      }
    }
  }
  # first part of the cleaning procedure for data in format 3
  if (qualt_data_format == 3) {
    # qualtrics format as of dec 25 2020
    # functions skipped
    functions_skipped <- c()
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
      default_cols_by_qualtrics_new <- paste0("qualt_", c(
        "start_date", "end_date", "status", "ip_address", "progress",
        "duration_in_seconds", "finished", "recorded_date",
        "response_id", "recipient_last_name", "recipient_first_name",
        "recipient_email", "external_ref", "location_latitude",
        "location_longitude", "dist_channel", "user_language"))
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
        "Following changes were made to the data set.\n\n",
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
      if (grep("ImportId", dt[["qualt_start_date"]]) == 2 &
          grep("ImportId", dt[["qualt_end_date"]]) == 2) {
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
  }
  ###################################################################
  #
  # the second part of the cleaning procedure common to all formats
  #
  ###################################################################
  # warn accuracy loss when converting numbers?
  type_convert_numerals_arg <- ifelse(
    warn_accuracy_loss == TRUE, "warn.loss", "allow.loss")
  # convert to numeric
  stopifnot(is.list(dt))
  number_of_all_cols <- length(dt)
  number_of_char_cols_before <- sum(
    vapply(dt, class, FUN.VALUE = character(1L)) == "character")
  for (col in names(dt)) {
    set(dt, j = col, value = utils::type.convert(
      dt[[col]], classes = "character", how = "replace",
      as.is = TRUE, numerals = type_convert_numerals_arg))
  }
  # report on converting to numeric
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
      dt <- dt[, (grep("_Page Submit$", names(dt))) := NULL][]
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
      # take a snapshot of the current column names
      col_name_dt <- data.table(
        old_col_name = names(dt),
        new_col_name = make.unique(names(dt))
      )
      # temporarily change column names
      names(dt) <- col_name_dt[["new_col_name"]]
      # move columns
      setcolorder(dt, names(dt)[grep(
        "_Page Submit$|_Page Submit\\.\\d+$", names(dt), invert = TRUE)])
      # change column names back to the old names
      setnames(dt, old = col_name_dt[["new_col_name"]],
               new = col_name_dt[["old_col_name"]],
               skip_absent = TRUE)
      # change names to snake case
      names(dt)[grep(
        "_Page Submit$|_Page Submit\\.\\d+$", names(dt))] <-
        gsub("_Page Submit", "_page_submit",
             names(dt)[grep(
               "_Page Submit$|_Page Submit\\.\\d+$", names(dt))])
      # update change id
      change_id <- change_id + 1
      # report
      message(paste0(
        change_id,
        ". Number of columns with page submit data that were moved ",
        "to the right with names converted to snake_case: ",
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
  # convert workerId etc column names,
  # the three "query string parameters" from CloudResearch to snake case
  # indices of CloudResearch column names
  cloud_research_col_indices <- grep(
    "^workerId|assignmentId|hitId$", names(dt))
  if (length(cloud_research_col_indices) > 0) {
    # if cloud research columns are right next to each other,
    # (i.e., if the cloud research columns were most likely set by
    # CloudResarch), change the column names to snake case
    if (all(abs(diff(cloud_research_col_indices)) == 1)) {
      setnames(
        dt,
        old = c("workerId", "assignmentId", "hitId"),
        new = c("cloud_research_worker_id",
                "cloud_research_assignment_id",
                "cloud_research_hit_id"),
        skip_absent = TRUE)
      # update change id
      change_id <- change_id + 1
      # report
      message(paste0(
        change_id,
        '. Names of Cloud Research columns (e.g., "workerId") were ',
        "converted to snake_case.\n"))
    }
  }
  # check if qualt_dist_channel column exists
  if ("qualt_dist_channel" %in% names(dt)) {
    # remove survey preview data
    if (remove_survey_preview_data == TRUE) {
      # current number of rows
      number_of_remaining_rows <- nrow(dt)
      # remove data from survey preview
      dt <- dt[qualt_dist_channel != "preview"]
      nrow_of_preview_data <- number_of_remaining_rows - nrow(dt)
      if (nrow_of_preview_data > 0) {
        # update change id
        change_id <- change_id + 1
        # report
        message(paste0(
          change_id,
          ". Number of rows from survey preview that were removed: ",
          nrow_of_preview_data,
          " out of ",
          number_of_remaining_rows,
          ".\n"))
      }
    }
    # remove test response data
    if (remove_test_response_data == TRUE) {
      # current number of rows
      number_of_remaining_rows <- nrow(dt)
      # remove data from test response
      dt <- dt[qualt_dist_channel != "test"]
      nrow_of_test_resp_data <- number_of_remaining_rows - nrow(dt)
      if (nrow_of_test_resp_data > 0) {
        # update change id
        change_id <- change_id + 1
        # report
        message(paste0(
          change_id,
          ". Number of rows from test response that were removed: ",
          nrow_of_test_resp_data,
          " out of ",
          number_of_remaining_rows,
          ".\n"))
      }
    }
  }
  # report functions not run
  if (change_id == 0) {
    message("No change was made to the data set.\n")
  }
  # return
  invisible(dt)
}
