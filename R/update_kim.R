#' Update 'kim'
#'
#' Updates the current package 'kim' by installing the most recent version
#'
#' @param source location of the most recent version of the package
#' (default = "github")
#' @param force logical. If \code{force = TRUE}, forces the installation
#' even if the package 'kim' from the source has not changed since
#' last install.
#' @param upgrade input for \code{upgrade} argument to be passed on to
#' \code{remotes::install_github}.
#' The default value is \code{FALSE}.
#' An input could be \code{TRUE} or \code{FALSE}, but it could also
#' be one of the following: "default", "ask", "always", or "never".
#' "default" respects the value of the R_REMOTES_UPGRADE
#' environment variable if set, and falls back to "ask" if unset.
#' "ask" prompts the user for which out of date packages to upgrade.
#' For non-interactive sessions "ask" is equivalent to "always".
#' TRUE and FALSE are also accepted and correspond
#' to "always" and "never" respectively.
#'
#' @examples
#' \dontrun{
#' update_kim()
#' }
#'
#' @export
#' @import devtools
update_kim <- function(
  source = "github",
  force = FALSE,
  upgrade = FALSE) {
  # unload the package kim
  while ("package:kim" %in% search()) {
    detach("package:kim", unload = TRUE, character.only = TRUE)
  }
  # if source is github
  if (source == "github") {
    # discourage updating
    # message(paste0(
    #   "\nIf you see below a long list of recommended packages to update,\n",
    #   "try updating none of them ",
    #   '(i.e., Type "3" in the console for "3: None")\n\n',
    #   "However, if you run into errors later using the package 'kim',\n",
    #   'try updating the relevant packages then.\n'
    # ))
    kim::prep("devtools", silent_if_successful = TRUE)
    devtools::install_github(
      "jinkim3/kim", force = force, upgrade = upgrade)
  }
  kim::prep("kim", silent_if_successful = TRUE)
}
