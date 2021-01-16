#' Read a csv file
#'
#' Read a csv file
#'
#' @param name a character string of the csv file name without the
#' ".csv" extension. For example, if the csv file to read is "myfile.csv",
#' enter \code{name = "myfile"}
#' @param head logical. if \code{head = TRUE}, prints the first five
#' rows of the data set.
#' @return the output will be a data.table object, that is,
#' an output from the data.table function, \code{fread}
#' @examples
#' \dontrun{
#' mydata <- read_csv("myfile")
#' }
#' @export
read_csv <- function(
  name = NULL,
  head = FALSE) {
  # check the name argument
  if (is.null(name)) {
    stop('Please enter a name of the csv file (e.g., name = "filename").')
  }
  # print the working directory
  message(paste0("\nCurrent working directory:\n", getwd(), "\n"))
  # vector of file names
  file_name_vector <- list.files()
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
  # read csv
  if (csv_file_name %in% file_name_vector) {
    dt <- data.table::fread(input = csv_file_name)
    if (head == TRUE) {
      message("First five (or fewer) rows of the data:")
      print(utils::head(dt, n = 5L))
    }
    message(paste0(
      "The following csv file was read from the working directory:\n",
      csv_file_name, "\n"))
    return(dt)
  } else {
    stop(csv_file_name, " does not exist in the working directory.")
  }
}
