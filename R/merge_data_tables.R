#' Merge data tables
#'
#' Merge two data.table objects. If there are any duplicated
#' ID values and column names across the two data tables, the
#' cell values in the first data.table will remain intact and
#' the cell values in the second data.table will be discarded for the
#' resulting merged data table.
#'
#' This function was edited using ChatGPT on Jun 10, 2025.
#' But not all of ChatGPT's edit suggestions were incorporated.
#'
#' @param dt1 the first data.table which will remain intact
#' @param dt2 the second data.table which will be joined outside of
#' (around) the first data.table. If there are any duplicated
#' ID values and column names across the two data tables, the
#' cell values in the first data.table will remain intact and
#' the cell values in the second data.table will be discarded for the
#' resulting merged data table.
#' @param id name(s) of the column(s) that will contain the ID values
#' in the two data tables. The name(s) of the ID column(s) must be identical
#' in the two data tables.
#' @param silent If \code{silent = TRUE}, no message will be printed
#' regarding how many ID values and column names were duplicated.
#' If \code{silent = FALSE}, messages will be printed regarding
#' how many column names were duplicated.
#' In cases where only one column was used as the 'id' column (which is the
#' most common case), \code{silent = FALSE} will also print messages
#' regarding how many input ID values were duplicated.
#' By default, \code{silent = FALSE}.
#' @return a data.table object, which merges (joins) the second data.table
#' around the first data.table.
#' @examples
#' ## Example 1: Typical Usage
#' data_1 <- data.table::data.table(
#' id_col = c(4, 2, 1, 3),
#' a = 3:6,
#' b = 5:8,
#' c = c("w", "x", "y", "z"))
#' data_2 <- data.table::data.table(
#' id_col = c(1, 99, 4),
#' e = 6:8,
#' b = c("p", "q", "r"),
#' d = c(TRUE, FALSE, FALSE))
#' # check the two example data tables
#' data_1
#' data_2
#' # check the result of merging the two data tables above and
#' # note how data_1 (the upper left portion) is intact in the resulting
#' # data table
#' merge_data_tables(dt1 = data_1, dt2 = data_2, id = "id_col")
#' # compare the result with above with the result from the `merge` function
#' merge(data_1, data_2, by = "id_col", all = TRUE)
#' ## Example 2: Some values can be converted
#' data_3 <- data.table::data.table(
#' id_col = 99,
#' a = "abc",
#' b = TRUE,
#' c = TRUE)
#' data_1
#' data_3
#' merge_data_tables(data_1, data_3, id = "id_col")
#' # In the example above, note how the value of TRUE gets
#' # converted to 1 in the last row of Column 'b' in the resulting data table
#' ## Example 3: A simpler case
#' data_4 <- data.table::data.table(
#' id_col = c(5, 3),
#' a = c("a", NA))
#' data_5 <- data.table::data.table(
#' id_col = 1,
#' a = 2)
#' # check the two example data tables
#' data_4
#' data_5
#' merge_data_tables(data_4, data_5, id = "id_col")
#' ## Example 4: Merging data tables using multiple ID columns
#' data_6 <- data.table::data.table(
#' id_col_1 = 3:1,
#' id_col_2 = c("a", "b", "c"),
#' id_col_3 = 4:6,
#' a = 7:9,
#' b = 10:12)
#' data_7 <- data.table::data.table(
#' id_col_1 = c(3, 2),
#' id_col_3 = c(3, 5),
#' id_col_2 = c("a", "b"),
#' c = 13:14,
#' a = 15:16)
#' # check the example data sets
#' data_6
#' data_7
#' # merge data sets using the three id columns
#' suppressWarnings(merge_data_tables(
#' dt1 = data_6,
#' dt2 = data_7,
#' id = c("id_col_1", "id_col_2", "id_col_3")))
#' @import data.table
#' @export
merge_data_tables <- function(
  dt1 = NULL,
  dt2 = NULL,
  id = NULL,
  silent = TRUE
) {
  # stop if an input is missing
  if (is.null(dt1)) {
    stop("Please enter an input for the first data table argument, dt1.")
  }
  if (is.null(dt2)) {
    stop("Please enter an input for the second data table argument, dt2.")
  }
  if (is.null(id)) {
    stop(paste0("Please enter an input for the ID column name, ",
                'e.g., id = "subject_id".'))
  }
  # copy and coerce inputs into data tables
  dt1c <- data.table::setDT(data.table::copy(dt1))
  dt2c <- data.table::setDT(data.table::copy(dt2))
  # check the id column for cases when there is only one id column
  if (length(id) == 1) {
    # check if the id column is in both data tables
    if (!id %in% names(dt1c)) {
      stop(paste0(
        'The id column "', id, '" is not found in the first',
        " data table, dt1."))
    }
    if (!id %in% names(dt2c)) {
      stop(paste0(
        'The id column "', id, '" is not found in the second',
        " data table, dt2."))
    }
  }
  # check the id column for cases when there multiple id columns
  if (length(id) > 1) {
    if (any(duplicated(id))) {
      stop("Each id column name must be unique.")
    }
    # check whether all the id columns exist in dt1
    id_cols_not_in_dt1 <- setdiff(id, names(dt1c))
    if (length(id_cols_not_in_dt1) > 0) {
      stop(paste0(
        "The following id column(s) are not found in the ",
        "first data table, dt1:\n",
        paste0(id_cols_not_in_dt1, collapse = ", ")))
    }
    # check whether all the id columns exist in dt2
    id_cols_not_in_dt2 <- setdiff(id, names(dt2c))
    if (length(id_cols_not_in_dt2) > 0) {
      stop(paste0(
        "The following id column(s) are not found in the ",
        "second data table, dt2:\n",
        paste0(id_cols_not_in_dt2, collapse = ", ")))
    }
  }
  # create a temporary id column when multiple columns
  # are given as input for id
  if (length(id) > 1) {
    i <- 1
    suffix_found <- FALSE
    while(suffix_found == FALSE) {
      if (!paste0("temp_col_name_kim_", i) %in% names(dt1c) &
          !paste0("temp_col_name_kim_", i) %in% names(dt2c)) {
        temp_id_col_name <- paste0("temp_col_name_kim_", i)
        suffix_found <- TRUE
      } else {
        i <- i + 1
      }
    }
    # use a timestamp-based separator unlikely to appear in ID values
    separator <- gsub("\\.", "", format(Sys.time(), "_%Y%m%d%H%M%OS6_"))
    dt1c[, (temp_id_col_name) :=
           do.call(paste, c(.SD, sep = separator)), .SDcols = id]
    # check the temporary id column just created for dt1
    if (any(duplicated(dt1c[[temp_id_col_name]]))) {
      stop(paste0(
        "Failed to create unique temporary ID values in dt1, ",
        "based on the\ninput of ID columns. This is likely due to ",
        "the function's limitations."))
    }
    dt2c[, (temp_id_col_name) :=
           do.call(paste, c(.SD, sep = separator)), .SDcols = id]
    # check the temporary id column just created for dt2
    if (any(duplicated(dt2c[[temp_id_col_name]]))) {
      stop(paste0(
        "Failed to create unique temporary ID values in dt2, ",
        "based on the\ninput of ID columns. This is likely due to ",
        "the function's limitations."))
    }
    # id values in each data table
    dt1c_id_values <- dt1c[[temp_id_col_name]]
    dt2c_id_values <- dt2c[[temp_id_col_name]]
  } else if (length(id) == 1) {
    # id values in each data table
    dt1c_id_values <- dt1c[[id]]
    dt2c_id_values <- dt2c[[id]]
  }
  # id values in the final dt
  id_in_final_dt <- union(dt1c_id_values, dt2c_id_values)
  # id values unique in dt2c
  unique_id_in_dt2c <- setdiff(dt2c_id_values, dt1c_id_values)
  # print duplicated id values
  duplicated_id_values <- intersect(dt1c_id_values, dt2c_id_values)
  if (length(id) == 1) {
    if (silent == FALSE) {
      message(paste0(
        "Number of duplicated ID values: ", length(duplicated_id_values)))
    }
  }
  # column names in each data table
  dt1c_col_names <- names(dt1c)
  dt2c_col_names <- names(dt2c)
  # column names in the final dt
  if (length(id) == 1) {
    col_names_in_final_dt <- union(dt1c_col_names, dt2c_col_names)
  } else if (length(id) > 1) {
    col_names_in_final_dt <- setdiff(
      union(dt1c_col_names, dt2c_col_names), temp_id_col_name)
  }
  # names of non-ID columns that are in both data tables
  if (length(id) == 1) {
    duplicated_col_names <- setdiff(
      intersect(dt1c_col_names, dt2c_col_names), id)
  } else if (length(id) > 1) {
    duplicated_col_names <- setdiff(
      intersect(dt1c_col_names, dt2c_col_names), temp_id_col_name)
  }
  # print duplicated column names
  if (silent == FALSE) {
    message(paste0(
      "Number of duplicated column names: ",
      length(duplicated_col_names)))
  }
  # set keys in each dt
  if (length(id) == 1) {
    data.table::setkeyv(dt1c, id)
    data.table::setkeyv(dt2c, id)
  } else if (length(id) > 1) {
    data.table::setkeyv(dt1c, temp_id_col_name)
    data.table::setkeyv(dt2c, temp_id_col_name)
  }
  # merge data tables
  merged_dt <- merge(dt1c, dt2c, all = TRUE, sort = FALSE)
  # merge duplicated columns
  if (length(duplicated_col_names) > 0) {
    if (length(id) == 1) {
      merged_cols <- lapply(duplicated_col_names, function(x) {
        dt1c_rows <- merged_dt[[id]] %in% dt1c_id_values
        v1 <- merged_dt[[paste0(x, ".x")]][dt1c_rows]
        dt2c_rows <- merged_dt[[id]] %in% unique_id_in_dt2c
        v2 <- merged_dt[[paste0(x, ".y")]][dt2c_rows]
        output <- c(v1, v2)
        return(output)
      })
    } else if (length(id) > 1) {
      merged_cols <- lapply(duplicated_col_names, function(x) {
        dt1c_rows <- merged_dt[[temp_id_col_name]] %in% dt1c_id_values
        v1 <- merged_dt[[paste0(x, ".x")]][dt1c_rows]
        dt2c_rows <- merged_dt[[temp_id_col_name]] %in% unique_id_in_dt2c
        v2 <- merged_dt[[paste0(x, ".y")]][dt2c_rows]
        output <- c(v1, v2)
        return(output)
      })
    }
    # give names for merged_cols
    names(merged_cols) <- duplicated_col_names
    # replace the first set of duplicated columns (those with suffix ".x")
    # with the newly created merged columns
    cols_to_replace <- paste0(duplicated_col_names, ".x")
    for (col in cols_to_replace) {
      data.table::set(
        merged_dt, j = col,
        value = merged_cols[[gsub("\\.x$", "", col)]])
    }
    # remove the second set of duplicated columns (those with suffix ".y")
    cols_to_remove <- paste0(duplicated_col_names, ".y")
    merged_dt[, (cols_to_remove) := NULL]
    data.table::setnames(
      merged_dt, old = cols_to_replace, duplicated_col_names)
  }
  # chatgpt suggested replacing the commented out section 1 below w section 2
  # section 1 begins here
  # # restore the original order of rows
  # if (length(id) == 1) {
  #   output <- kim::order_rows_specifically_in_dt(
  #     dt = merged_dt,
  #     col_to_order_by = id,
  #     specific_order = id_in_final_dt)
  # } else if (length(id) > 1) {
  #   output <- kim::order_rows_specifically_in_dt(
  #     dt = merged_dt,
  #     col_to_order_by = temp_id_col_name,
  #     specific_order = id_in_final_dt)
  #   # remove the temporary id column
  #   output[, (temp_id_col_name) := NULL][]
  # }
  # section 1 ends here
  # section 2 begins here
  # restore the original order of rows and set 'sorted' attribute
  if (length(id) == 1) {
    data.table::setorderv(merged_dt, id)
    output <- merged_dt
  } else {
    data.table::setorderv(merged_dt, temp_id_col_name)
    merged_dt[, (temp_id_col_name) := NULL]
    output <- merged_dt
  }
  # restore key if both dt1 and dt2 had the same key
  if (identical(data.table::key(dt1), id) &&
      identical(data.table::key(dt2), id)) {
    data.table::setkeyv(output, id)
  }
  # section 2 ends here
  # output
  return(output)
}
