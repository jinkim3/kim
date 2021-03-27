#' Update the package 'kim'
#'
#' Updates the current package 'kim' by installing the
#' most recent version of the package from GitHub
#'
#' @param confirm logical. If \code{confirm = TRUE}, the user will
#' need to confirm the update. If \code{confirm = FALSE}, the confirmation
#' step will be skipped. By default, \code{confirm = TRUE}.
#' @return there will be no output from this function. Rather, executing
#' this function will update the current 'kim' package by installing
#' the most recent version of the package from GitHub.
#' @examples
#' \dontrun{
#' if (interactive()) {update_kim()}
#' }
#'
#' @export
update_kim <- function(
  confirm = TRUE) {
  # 5 possible cases
  # 1. error in getting the current package version -> yes
  # 2. error in getting the github package version -> yes
  # 3. current package version < github package version -> yes
  # 4. current package version > github package version -> no
  # 5. current package version == github package version -> no
  # in short, notify the option to update unless the version numbers match

  # get version of the currently installed package
  current_pkg_version <- tryCatch(
    as.character(utils::packageVersion("kim")),
    error = function(e) "unknown")
  # github url
  github_url <- paste0(
    "https://raw.githubusercontent.com/jinkim3/",
    "kim/master/DESCRIPTION")
  # get github description or handle errors
  github_pkg_desc <- tryCatch(
    readLines(github_url),
    warning = function(w) {"github_desc_read_fail"},
    error = function(e) {"github_desc_read_fail"})
  # get the version number of github version
  if (identical(github_pkg_desc, "github_desc_read_fail")) {
    github_pkg_version <- "unknown"
  } else {
    github_pkg_version <- gsub(
      ".*ersion: ", "", github_pkg_desc[
        grep("ersion", github_pkg_desc)])
  }
  # compare versions
  compare_version_result <- tryCatch(
    utils::compareVersion(
      current_pkg_version, github_pkg_version),
    warning = function(w) {999}, # 999 indicates no need for update
    error = function(e) {999})
  # skip update for case 5
  if (current_pkg_version != "unknown" &
      github_pkg_version != "unknown" &
      compare_version_result == 0) {
    update_result_message <- paste0(
      "Current version of 'kim': v", current_pkg_version,
      " (same as the most recent version available through GitHub).")
  } else if (
    # skip update for case 4
    current_pkg_version != "unknown" &
    github_pkg_version != "unknown" &
    compare_version_result > 0) {
    update_result_message <- paste0(
      "Current version of 'kim': v", current_pkg_version,
      " (probably the most recent version available through GitHub).")
  } else {
    # confirm update
    if (confirm == TRUE) {
      # ask the user to confirm
      user_reply <- utils::menu(
        c("Yes.", "No."),
        title = "\nDo you want to try to update the package 'kim'?")
    } else {
      # if not asked, assume the user wants to update
      user_reply <- 1
    }
    # update if user wants the update
    if (user_reply == 1) {
      # unload the package kim
      while ("package:kim" %in% search()) {
        unloadNamespace("kim")
      }
      remotes::install_github("jinkim3/kim")
      # attach the package
      kim::prep("kim", silent_if_successful = TRUE)
    }
  }
}
