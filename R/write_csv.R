#' Write to a csv file
#'
#' Write to a csv file
#'
#' @param data a data object (a data frame or a data.table)
#' @param name a character string of the csv file name without the
#' ".csv" extension. For example, if the csv file to write to is
#' "myfile.csv", enter \code{name = "myfile"}
#' @param timestamp logical. Should the timestamp be appended to the
#' file name?
#' @return the output will be a .csv file in the working directory,
#' that is, an output from the data.table function, \code{fwrite}
#' @examples
#' \dontrun{
#' write_csv(mtcars, "mtcars_from_write_csv")
#' write_csv(mtcars)
#' }
#' @export
write_csv <- function(
  data = NULL,
  name = NULL,
  timestamp = NULL) {
  # check the data argument
  if (is.data.frame(data) == FALSE) {
    stop(paste0(
      "The data input is not a data frame. ",
      "Please give a data frame as the input."))
  }
  # set a default value for the timestamp argument
  if (is.null(timestamp)) {
    # if file name is not given, then add the timestamp by default
    if (is.null(name)) {
      timestamp <- TRUE
    } else {
      # if file name is given, then do not add the timestamp by default
      timestamp <- FALSE
    }
  }
  # set name if none is given
  if (is.null(name)) {
    name <- as.character(substitute(data))
  }
  # create a timestamp
  if (timestamp == TRUE) {
    name_w_or_wo_timestamp <- paste0(
      name, tolower(
        gsub("\\.", "_", format(Sys.time(), "_%b_%d_%Y_%H%M_%OS6"))))
  } else {
    name_w_or_wo_timestamp <- name
  }
  # check if the name given has a file extension
  if (grepl("\\.", name)) {
    # check file extension
    possible_file_extension <-
      tolower(utils::tail(strsplit(name, "\\.")[[1]], 1L))
  } else {
    possible_file_extension <- ""
  }
  # add .csv to the name if necessary
  if (possible_file_extension != "csv") {
    csv_file_name <- paste0(name_w_or_wo_timestamp, ".csv")
  } else {
    csv_file_name <- name_w_or_wo_timestamp
  }
  # write to a csv
  data.table::fwrite(data, file = csv_file_name)
}
