#' Check for required packages
#'
#' Check whether required packages are installed.
#'
#' @param pkg a character vector containing names of packages to check
#' @return there will be no output from this function. Rather, the
#' function will check whether the packages given as inputs are installed.
#' @examples
#' \donttest{
#' check_req_pkg("data.table")
#' check_req_pkg(c("base", "utils", "ggplot2", "data.table"))
#' }
#'
#' @export
check_req_pkg <- function(
  pkg = NULL) {
  # check whether the pkg argument is a character vector
  if (is.character(pkg) == FALSE) {
    stop("The 'pkg' argument must be a character vector.")
  }
  # currently installed packages
  installed_packages <- rownames(utils::installed.packages())
  # currently uninstalled packages
  uninstalled_packages <- setdiff(pkg, installed_packages)
  # report all of the queried packages are installed
  if (all(pkg %in% installed_packages)) {
    message(paste0(
      "All of the following packages were already installed:\n",
      paste0(pkg, collapse = ", ")))
  }
  # report which packages need to be installed
  if (length(uninstalled_packages) > 0) {
    message(paste0(
      "The following packages were not installed:\n",
      paste0(uninstalled_packages, collapse = ", "),
      "\nType 'kim::prep(",
      paste0(uninstalled_packages, collapse = ", "),
      ")' to install and load the above packages."))
  }
}
