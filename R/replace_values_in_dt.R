#' Replace values in a data table
#'
#' Replace values in a data.table
#'
#' @param data a data object (a data frame or a data.table)
#' @param old_values a vector of old values that need to be replaced
#' @param new_values a new value or a vector of new values that will
#' replace the old values
#' @param silent If \code{silent = FALSE}, a message will be printed regarding
#' how many values were replaced. If \code{silent = TRUE}, no message
#' will be printed regarding how many values were replaced. (default = FALSE)
#' @examples
#' replace_values_in_dt(data = mtcars, old_values = 21.0, new_values = 888)
#' replace_values_in_dt(data = mtcars, old_values = c(0, 1), new_values = 999)
#' replace_values_in_dt(
#' data = mtcars, old_values = c(0, 1), new_values = 990:991)
#' replace_values_in_dt(
#' data = data.table::data.table(a = NA_character_, b = NA_character_),
#' old_values = NA, new_values = "")
#' @export
replace_values_in_dt <- function(
  data = NULL,
  old_values = NULL,
  new_values = NULL,
  silent = FALSE) {
  # copy data
  dt <- data.table::setDT(data.table::copy(data))
  # check the old values input
  if (is.null(old_values)) {
    stop(paste0(
      "Please enter a single value or a vector of values for ",
      'the argument, "old_values".'))
  }
  # check that if there are multiple new values, the number of new values
  # match the number of old values
  if (length(new_values) > 1) {
    if (length(new_values) != length(old_values)) {
      stop(paste0("The number of new_values must be either 1 or ",
                  "equal to the number of old_values."))
    }
  }
  # replace old value(s) with a single new value
  if (length(new_values) == 1) {
    # check if the old values are all na
    if (all(is.na(unique(old_values)))) {
      count_of_old_values <- sum(is.na(dt))
      if (count_of_old_values > 0) {
        # check for duplicated columns
        if (any(duplicated(names(dt)))) {
          kim::pm(
            "Applying a slower function due to duplicates ",
            "in column names...")
          dt[is.na(dt)] <- new_values
        }
        # probably a faster process
        for (col in names(dt)) {
          data.table::set(
            dt, i = which(is.na(dt[[col]])), j = col, value = new_values)
        }
      }
    } else {
      # replace the single, specific old value
      if (length(old_values) == 1) {
        count_of_old_values <- sum(dt == old_values, na.rm = TRUE)
        if (count_of_old_values > 0) {
          # check for duplicated columns
          if (any(duplicated(names(dt)))) {
            kim::pm(
              "Applying a slower function due to duplicates ",
              "in column names...")
            dt[dt == old_values] <- new_values
          }
          # probably a faster process
          for (col in names(dt)) {
            data.table::set(
              dt, i = which(dt[[col]] == old_values),
              j = col, value = new_values)
          }
        }
      }
      # replace multiple old values with a single new value
      if (length(old_values) > 1) {
        count_list <- lapply(old_values, function(x) {
          sum(dt == old_values, na.rm = TRUE)
        })
        count_of_old_values <- sum(unlist(count_list))
        if (count_of_old_values > 0) {
          # check for duplicated columns
          if (any(duplicated(names(dt)))) {
            kim::pm(
              "Applying a slower function due to duplicates ",
              "in column names...")
            dt[dt %in% old_values] <- new_values
          }
          # probably a faster process
          for (col in names(dt)) {
            data.table::set(
              dt, i = which(dt[[col]] %in% old_values), j = col,
              value = new_values)
          }
        }
      }
    }
  }
  # replace multiple old values with multiple new values
  if (length(new_values) > 1) {
    # count the number of values to be replaced
    count_of_old_values <- sum(unlist(dt) %in% old_values)
    # create a reference data set for using the vlookup function
    ref <- data.table::data.table(old_values, new_values)
    for (i in seq_along(dt)) {
      kim::print_loop_progress(i, iteration_end = length(dt))
      focal_col <- dt[, get(names(dt)[i])]
      if (length(intersect(focal_col, old_values)) > 0) {
        rest_of_the_values <- setdiff(focal_col, old_values)
        if (length(rest_of_the_values) == 0) {
          data.table::set(
            dt, j = names(dt)[i], value = kim::vlookup(
              lookup_values = focal_col,
              reference_dt = ref,
              col_name_for_lookup_values = "old_values",
              col_name_for_output_values = "new_values"))
        } else if (length(rest_of_the_values) > 0) {
          ref_2 <- rbind(ref, data.table::data.table(
            old_values = rest_of_the_values,
            new_values = rest_of_the_values))
          data.table::set(
            dt, j = names(dt)[i], value = kim::vlookup(
              lookup_values = focal_col,
              reference_dt = ref_2,
              col_name_for_lookup_values = "old_values",
              col_name_for_output_values = "new_values"))
        }
      }
    }
  }
  # report summary
  if (silent == FALSE) {
    message("A total of ", count_of_old_values, " value(s) were replaced.")
  }
  # return the output
  return(dt)
}
