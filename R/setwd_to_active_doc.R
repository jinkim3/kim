#' Set working directory to active document in RStudio
#'
#' Set working directory to location of the active document in RStudio
#'
#' @return there will be no output from this function. Rather, the
#' working directory will be set as location of the active document.
#' @examples
#' \dontrun{
#' setwd_to_active_doc()
#' }
#' @export
setwd_to_active_doc <- function() {
  # check if the user is in RStudio
  if (Sys.getenv("RSTUDIO") == 1) {
    # function to get the active document, if the user is using tools
    fn_to_get_active_doc <- get(
      ".rs.api.getActiveDocumentContext",
      envir = as.environment(match("tools:rstudio", search())))
    # try setting working directory
    setwd_result <- tryCatch(
      {setwd(dirname(fn_to_get_active_doc()$path))
        "success"},
      error = function(e) {"error"},
      warning = function(w) {"warning"})
    # if success
    if (setwd_result == "success") {
      message(paste0(
        "The working directory has been set as location of the",
        " active document:\n"))
    } else {
      message(paste0(
        "The working directory was NOT set to location of the",
        " active document.\nPlease try the function ",
        "`setwd_to_active_doc()` or manually set the working directory."))
    }
    cat(paste0(getwd(), "\n\n"))
  } else {
    message(paste0(
      "This function can only be run on RStudio.\n",
      "Please manually set the working directory."))
  }
}
