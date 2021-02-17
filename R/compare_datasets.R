#' Compare data sets
#'
#' Compares whether or not data sets are identical
#'
#' @param dataset_1 a data object (a data frame or a data.table)
#' @param dataset_2 another data object (a data frame or a data.table)
#' @param dataset_list list of data objects (data.frame or data.table)
#' @return the output will be a data.table showing differences in data sets
#' @examples
#' # catch differences in class attributes of the data sets
#' compare_datasets(
#' dataset_1 = data.frame(a = 1:2, b = 3:4),
#' dataset_2 = data.table::data.table(a = 1:2, b = 3:4))
#' # catch differences in number of columns
#' compare_datasets(
#' dataset_1 = data.frame(a = 1:2, b = 3:4, c = 5:6),
#' dataset_2 = data.frame(a = 1:2, b = 3:4))
#' # catch differences in number of rows
#' compare_datasets(
#' dataset_1 = data.frame(a = 1:2, b = 3:4),
#' dataset_2 = data.frame(a = 1:10, b = 11:20))
#' # catch differences in column names
#' compare_datasets(
#' dataset_1 = data.frame(A = 1:2, B = 3:4),
#' dataset_2 = data.frame(a = 1:2, b = 3:4))
#' # catch differences in values within corresponding columns
#' compare_datasets(
#' dataset_1 = data.frame(a = 1:2, b = c(3, 400)),
#' dataset_2 = data.frame(a = 1:2, b = 3:4))
#' compare_datasets(
#' dataset_1 = data.frame(a = 1:2, b = 3:4, c = 5:6),
#' dataset_2 = data.frame(a = 1:2, b = c(3, 4), c = c(5, 6)))
#' # check if data sets in a list are identical
#' compare_datasets(
#' dataset_list = list(
#' dt1 = data.frame(a = 1:2, b = 3:4, c = 5:6),
#' dt2 = data.frame(a = 1:2, b = 3:4),
#' dt3 = data.frame(a = 1:2, b = 3:4, c = 5:6)))
#' @import data.table
#' @importFrom utils head
#' @export
compare_datasets <- function(
  dataset_1 = NULL,
  dataset_2 = NULL,
  dataset_list = NULL) {
  # bind the vars locally to the function
  ..different_vs_same <- NULL
  # copy data sets
  dt1 <- copy(dataset_1)
  dt2 <- copy(dataset_2)
  # check inputs
  if (is.null(dataset_list) & !is.null(dt1) & !is.null(dt2)) {
    dt_list <- list(dt1, dt2)
  } else if (
    !is.null(dataset_list) & is.null(dt1) & is.null(dt2)) {
    if (!is.list(dataset_list)) {
      stop(paste0("The input for dataset_list is not a list."))
    }
    dt_list <- dataset_list
  } else {
    stop(paste0(
      "Please check the data set inputs.\nEither enter a list of ",
      "data sets, e.g., dataset_list = list(mtcars, iris)\n",
      "or enter two data sets, e.g., dt1 = mtcars, ",
      "dt2 = iris"))
  }
  # set names of the data sets
  if (is.null(names(dt_list))) {
    dataset_name <- seq_along(dt_list)
  } else {
    dataset_name <- names(dt_list)
  }
  # check class
  check_class_result <- data.table(
    dataset = dataset_name,
    class = vapply(dt_list, function(dt) {
      paste0(sort(class(dt)), collapse = ", ")
    }, FUN.VALUE = character(1L)))
  if (length(unique(check_class_result[["class"]])) > 1) {
    message('Data sets have different "class" attributes:')
    return(check_class_result)
  }
  # check number of columns
  check_ncol_result <- data.table(
    dataset = dataset_name,
    number_of_columns = vapply(dt_list, function(dt) {
      ncol(dt)
    }, FUN.VALUE = numeric(1L)))
  if (length(unique(check_ncol_result[["number_of_columns"]])) > 1) {
    message("Data sets have different number of columns:")
    return(check_ncol_result)
  }
  # check number of rows
  check_nrow_result <- data.table(
    dataset = dataset_name,
    number_of_rows = vapply(dt_list, function(dt) {
      nrow(dt)
    }, FUN.VALUE = numeric(1L)))
  if (length(unique(check_nrow_result[["number_of_rows"]])) > 1) {
    message("Data sets have different number of rows:")
    return(check_nrow_result)
  }
  # get column names of each data set
  colnames_by_dt <- lapply(dt_list, function(dt) {
    colnames(dt)
  })
  # fill with na if number of columns differs
  colnames_by_dt_max_row <- max(lengths(colnames_by_dt), na.rm = TRUE)
  colnames_by_dt <- lapply(colnames_by_dt, function(x) {
    if (length(x) < colnames_by_dt_max_row) {
      new_names <- c(x, rep(NA, colnames_by_dt_max_row - length(x)))
    } else {
      new_names <- x
    }
    return(new_names)
  })
  # check column names
  check_colnames_result <- setDT(colnames_by_dt)
  names(check_colnames_result) <- paste0("data set: ", dataset_name)
  # check which pairs of data sets have different column names
  same_colnames <- vapply(
    head(seq_len(ncol(colnames_by_dt)), -1), function(i) {
      identical(
        colnames_by_dt[[names(colnames_by_dt)[i]]],
        colnames_by_dt[[names(colnames_by_dt)[i + 1]]])
    }, FUN.VALUE = logical(1L))
  if (FALSE %in% same_colnames) {
    # find the first pair of data sets that have different column names
    index_of_dt_w_diff_colnames <-
      min(which(same_colnames == FALSE), na.rm = TRUE)
    # print column names for the first pair of data sets with different
    # column names
    message("The following data sets have different column names:")
    return(check_colnames_result[
      , .SD, .SDcols = c(
        index_of_dt_w_diff_colnames, index_of_dt_w_diff_colnames + 1)])
  }
  # check whether column classes match
  col_types_by_dt <- setDT(as.data.frame(do.call(
    rbind,
    lapply(seq_len(length(dt_list)), function(i) {
      vapply(dt_list[[i]], class, FUN.VALUE = character(1L))
    }))))
  cols_w_diff_class <- names(which(apply(
    col_types_by_dt, 2, function(x) length(unique(x))) > 1))
  if (length(cols_w_diff_class) > 0) {
    col_types_by_dt <- col_types_by_dt[
      , cols_w_diff_class, with = FALSE]
    names(col_types_by_dt) <- paste0("class_of_", cols_w_diff_class)
    col_types_by_dt <- data.table(
      dataset = dataset_name, col_types_by_dt)
    # print the different column types
    message(paste0(
      'The columnS with the name(s) "',
      paste0(cols_w_diff_class, collapse = ", "),
      '" were of different class ',
      "in the following two data sets:"))
    return(col_types_by_dt)
  }
  # check columns
  for (i in seq_along(dt_list[[1]])) {
    # get ith column from each of the data sets
    individual_cols <- lapply(dt_list, function(dt) {
      dt[[i]]
    })
    # check whether any pair among the set of ith columns is not identical
    for (j in head(seq_len(length(individual_cols)), -1)) {
      ind_col_identical <- identical(
        individual_cols[[j]], individual_cols[[j + 1]])
      if (ind_col_identical == FALSE) {
        check_ind_cols_result <- data.table(
          individual_cols[[j]],
          individual_cols[[j + 1]])
        names(check_ind_cols_result) <- paste0(
          "data set: ", dataset_name[j:(j + 1)])
        # label rows as having identical values or not
        check_ind_cols_result[
          , identical := c("not_identical", "identical")[
            (uniqueN(unlist(.SD)) == 1) + 1],
          by = seq_len(nrow(check_ind_cols_result))][]
        # print the pair of different columns
        message(paste0(
          'The column with the name "',
          names(dt_list[[1]])[i], '" was different ',
          "in the following two data sets:"))
        return(check_ind_cols_result)
      }
    }
  }
  # check whether data sets are identical
  for (i in head(seq_len(length(dt_list)), -1)) {
    dataset_identical <- identical(
      dt_list[[i]], dt_list[[i + 1]])
    if (dataset_identical == FALSE) {
      # print the pair of different columns
      message(paste0(
        "The following two data sets were not identical:\n",
        paste0(paste0("data set: ", dataset_name[i:(i + 1)]),
               collapse = "\n")))
      return()
    }
  }
  message("Wow, all of the data sets are identical!")
}
