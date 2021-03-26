#' Set up R environment
#'
#' Set up R environment by (1) clearing the console; (2) removing all
#' objects in the global environment; (3) setting the working directory
#' to the active document (in RStudio only); (4) unloading and
#' loading the kim package.
#'
#' @param clear_console if \code{TRUE}, clear the console (default = TRUE)
#' @param clear_global_env if \code{TRUE}, remove all objects in the
#' global environment (default = TRUE)
#' @param setwd_to_active_doc if \code{TRUE}, set the working
#' directory to the active document in RStudio (default = TRUE)
#' @param prep_kim if \code{TRUE}, unload and load the kim package
#' (default = TRUE)
#' @examples
#' \dontrun{
#' setup_r_env()
#' }
#' @export
setup_r_env <- function(
  clear_console = TRUE,
  clear_global_env = TRUE,
  setwd_to_active_doc = TRUE,
  prep_kim = TRUE
) {
  # clear console
  if (clear_console == TRUE) {
    if (Sys.getenv("RSTUDIO") == 1) {
      cat("\014")
      message("The console has been cleared.")
    } else {
      message("The console-clearing function works only in RStudio.")
    }
  }
  # clear objects in the global environment
  if (clear_global_env == TRUE) {
    rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
    message("All objects in the global environment has been removed.")
  }
  # set wd to the active document
  if (setwd_to_active_doc == TRUE) {
    setwd_to_active_doc()
  }
  # unload and load package kim
  if (prep_kim == TRUE) {
    # unload and attach the package kim
    while ("package:kim" %in% search()) {
      detach("package:kim", unload = TRUE, character.only = TRUE)
    }
    kim::prep("kim", silent_if_successful = TRUE)
  }
}
