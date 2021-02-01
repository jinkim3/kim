#' Start kim
#'
#' Start kim (update kim; attach default packages; set working directory, etc.)
#'
#' @param update logical. If \code{update = TRUE}, updates the current
#' package "kim" by installing the most recent version (probably from Github)
#' By default, \code{update = TRUE}
#' @param setup_r_env logical. If \code{update = TRUE}, runs the function
#' setup_r_env in the package "kim". Type "?kim::setup_r_env" to learn more.
#' By default, \code{setup_r_env = TRUE}
#' @param default_packages a vector of names of packages to load and attach.
#' By default, \code{default_packages = c("data.table", "ggplot2")}
#' @param silent_load_data_table logical. If
#' \code{silent_load_data_table = TRUE}, the data.table package will be
#' loaded and attached silently. If \code{silent_load_data_table = FALSE},
#' the data.table package will be loaded with its default message.
#' @examples
#' \dontrun{
#' start_kim()
#' start_kim(default_packages = c("dplyr", "ggplot2"))
#' start_kim(update = TRUE, setup_r_env = FALSE)
#' }
#' @export
start_kim <- function(
  update = TRUE,
  setup_r_env = TRUE,
  default_packages = c("data.table", "ggplot2"),
  silent_load_data_table = TRUE) {
  # update the package
  if (update == TRUE) {
    kim::update_kim()
  }
  # set up r env
  if (setup_r_env == TRUE) {
    kim::setup_r_env()
  }
  # default packages to attach
  if (length(default_packages) > 0) {
    # if data.table should be loaded, load it silently?
    if (silent_load_data_table == TRUE &
        "data.table" %in% default_packages) {
      # load data table silently
      suppressMessages(kim::prep("data.table"))
      # load other packages
      pkgs_other_than_data_table <-
        default_packages[default_packages != "data.table"]
      if (length(pkgs_other_than_data_table) > 0) {
        kim::prep(pkgs_other_than_data_table,
                  pkg_names_as_object = TRUE)
      }
    } else {
      kim::prep(default_packages, pkg_names_as_object = TRUE)
    }
  }
}
