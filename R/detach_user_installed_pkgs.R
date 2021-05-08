#' Detach all user-installed packages
#'
#' @param exceptions a character vector of names of packages to keep attached
#' @param force logical. Should a package be detached even though other
#' attached packages depend on it? By default, \code{force = FALSE}
#' @param keep_kim logical. If \code{keep_kim = FALSE}, Package 'kim'
#' will be detached along with all other user-installed packages.
#' If \code{keep_kim = TRUE}, Package 'kim' will not be detached.
#' By default, \code{keep_kim = FALSE}
#' @examples
#' \dontrun{
#' detach_user_installed_pkgs()
#' }
#' @export
detach_user_installed_pkgs <- function(
  exceptions = NULL,
  force = FALSE,
  keep_kim = TRUE
) {
  # currently attached user installed packages
  attached_user_installed_pkgs <- names(utils::sessionInfo()$otherPkgs)
  # keep kim?
  if (keep_kim == TRUE) {
    exceptions <- c(exceptions, "kim")
  }
  # remove the exceptions
  pkgs_to_detach <- setdiff(attached_user_installed_pkgs, exceptions)
  # detach packages
  if (!is.null(pkgs_to_detach)) {
    invisible(lapply(
      paste0("package:", pkgs_to_detach), detach,
      character.only = TRUE, force = force))
    message("The following package(s) were detached: ",
            paste0(pkgs_to_detach, collapse = ", "))
  } else {
    message("No packages were detached.")
  }
}
