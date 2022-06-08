#' Read a csv file
#'
#' Read a csv file
#'
#' @param name a character string of the csv file name without the
#' ".csv" extension. For example, if the csv file to read is "myfile.csv",
#' enter \code{name = "myfile"}
#' @param head logical. if \code{head = TRUE}, prints the first five
#' rows of the data set.
#' @param dirname a character string of the directory containing
#' the csv file, e.g., \code{dirname = "c:/Users/Documents"}
#' @param ... optional arguments for the \code{fread} function
#' from the data.table package. Any arguments for data.table's \code{fread}
#' function can be used, e.g., \code{fill = TRUE}, \code{nrows = 100}
#' @return the output will be a data.table object, that is,
#' an output from the data.table function, \code{fread}
#' @examples
#' \dontrun{
#' mydata <- read_csv("myfile")
#' }
#' @export
read_csv <- function(
    name = NULL,
    head = FALSE,
    dirname = NULL,
    ...) {
  # check the name argument; open a dialog if none is given
  if (is.null(name)) {
    message("Please select the file.")
    file_path <- file.choose()
    dt <- data.table::fread(input = file_path, ...)
    if (head == TRUE) {
      message("First five (or fewer) rows of the data:")
      print(utils::head(dt, n = 5L))
    }
    return(dt)
  }
  # folder description
  if (is.null(dirname)) {
    folder_description <- "working directory"
  } else {
    folder_description <- "specified folder"
  }
  # print the working directory
  if (is.null(dirname)) {
    message(paste0("\nCurrent working directory:\n", getwd(), "\n"))
  } else {
    message(paste0(
      "\nLocation searched:\n", dirname, "\n"))
  }
  # vector of file names
  if (is.null(dirname)) {
    file_name_vector <- list.files()
  } else {
    file_name_vector <- list.files(path = dirname)
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
    csv_file_name <- paste0(name, ".csv")
  } else {
    csv_file_name <- name
  }
  # add location if dirname is given
  if (is.null(dirname)) {
    fread_input_arg <- csv_file_name
  } else {
    fread_input_arg <- paste0(ifelse(
      endsWith(dirname, "/"), dirname, paste0(dirname, "/")),
      csv_file_name)
  }
  # read csv
  if (csv_file_name %in% file_name_vector) {
    dt <- data.table::fread(input = fread_input_arg, ...)
    if (head == TRUE) {
      message("First five (or fewer) rows of the data:")
      print(utils::head(dt, n = 5L))
    }
    message(paste0(
      "The following csv file was read:\n",
      csv_file_name, "\n"))
    return(dt)
  } else {
    stop(csv_file_name, " does not exist in the location above.")
  }
}
