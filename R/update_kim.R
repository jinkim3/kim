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
  # confirm update
  user_reply <- utils::menu(
    c("Yes.", "No."),
    title = "\nDo you want to try to update the package 'kim'?")
  if (user_reply == 1) {
    # unload the package kim
    while ("package:kim" %in% search()) {
      unloadNamespace("kim")
    }
    # if source is github
    remotes::install_github("jinkim3/kim")
    # attach the package
    kim::prep("kim", silent_if_successful = TRUE)
  }
}
