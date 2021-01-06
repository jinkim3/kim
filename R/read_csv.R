#' Read a csv file
#'
#' Read a csv file
#'
#' @param name a character string of the csv file name.
#' For example, if the csv file to read is "myfile.csv",
#' enter \code{name = "myfile"}
#' @param head logical. if \code{head = TRUE}, prints the first five
#' rows of the data set.
#' @return the output will be a data.table object, that is,
#' an output from the data.table function, \code{fread}
#' @examples
#' \donttest{
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
  # csv file name
  csv_file_name <- paste0(name, ".csv")
  # vector of csv file names
  name_of_file_to_read <- kim::regex_match(
    regex = paste0(name, ".csv|CSV$"),
    vector = file_name_vector,
    mute_report = TRUE)
  # check if there are more than one csv file
  if (length(name_of_file_to_read) > 1) {
    message(paste0(
      "No csv file was imported, because there are more than ",
      "one csv files with the name, '",
      csv_file_name, "':\n", paste0(name_of_file_to_read, collapse = ", ")))
  } else if (length(name_of_file_to_read) == 1) {
    message(paste0(
      "The following csv file was read from the working directory:\n",
      name_of_file_to_read, "\n"))
    dt <- data.table::fread(input = name_of_file_to_read)
    if (head == TRUE) {
      message("First five (or fewer) rows of the data:")
      print(utils::head(dt, n = 5L))
    }
    return(dt)
  } else {
    message(csv_file_name, " does not exist in the working directory.")
  }
}
