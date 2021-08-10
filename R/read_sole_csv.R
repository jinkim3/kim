#' Read the sole csv file in the working directory
#'
#' Read the sole csv file in the working directory
#'
#' @param head logical. if \code{head = TRUE}, prints the first five
#' rows of the data set.
#' @param ... optional arguments for the \code{fread} function
#' from the data.table package. Any arguments for data.table's \code{fread}
#' function can be used, e.g., \code{fill = TRUE}, \code{nrows = 100}
#' @return the output will be a data.table object, that is,
#' an output from the data.table function, \code{fread}
#' @examples
#' \donttest{
#' mydata <- read_sole_csv()
#' mydata <- read_sole_csv(head = TRUE)
#' mydata <- read_sole_csv(fill = TRUE, nrows = 5)
#' }
#' @export
read_sole_csv <- function(
  head = FALSE, ...) {
  # print the working directory
  message(paste0("\nCurrent working directory:\n", getwd(), "\n"))
  # vector of file names
  file_name_vector <- list.files()
  # vector of csv file names
  csv_file_name_vector <- kim::regex_match(
    regex = ".+\\.csv|CSV$",
    vector = file_name_vector,
    silent = TRUE)
  # check if there are more than one csv file
  if (length(csv_file_name_vector) > 1) {
    message(paste0(
      "No csv file was imported, because there are more than ",
      "one csv files in the working directory:\n",
      paste0(csv_file_name_vector, collapse = ", "),
      '\n\nTo open a specific csv file, try kim::read_csv("filename")\n'))
  } else if (length(csv_file_name_vector) == 1) {
    message(paste0(
      "The following csv file was read from the working directory:\n",
      csv_file_name_vector, "\n"))
    dt <- data.table::fread(input = csv_file_name_vector, ...)
    if (head == TRUE) {
      message("First five (or fewer) rows of the data:")
      print(utils::head(dt, n = 5L))
    }
    return(dt)
  } else {
    message("There is no csv file in the working directory.")
  }
}
