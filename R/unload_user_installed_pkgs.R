#' Unload all user-installed packages
#'
#' @param exceptions a character vector of names of packages to keep loaded
#' @param force logical. Should a package be unloaded even though other
#' attached packages depend on it? By default, \code{force = FALSE}
#' @param keep_kim logical. If \code{keep_kim = FALSE}, Package 'kim'
#' will be detached along with all other user-installed packages.
#' If \code{keep_kim = TRUE}, Package 'kim' will not be detached.
#' By default, \code{keep_kim = FALSE}
#' @examples
#' \dontrun{
#' unload_user_installed_pkgs()
#' }
#' @export
unload_user_installed_pkgs <- function(
  exceptions = NULL,
  force = FALSE,
  keep_kim = TRUE
) {
  # bind the vars locally to the function
  Priority <- Package <- NULL
  # get information on installed packages
  pkg_df <- as.data.frame(utils::installed.packages())
  # default packages (base or recommended)
  default_pkg <- subset(
    pkg_df, Priority %in% c("base", "recommended"))[["Package"]]
  # currently loaded user installed packages
  loaded_user_installed_pkgs <- setdiff(loadedNamespaces(), default_pkg)
  # keep kim?
  if (keep_kim == TRUE) {
    exceptions <- c(exceptions, "kim")
  }
  # remove the exceptions
  pkgs_to_unload <- setdiff(loaded_user_installed_pkgs, exceptions)
  # unload packages
  if (!is.null(pkgs_to_unload)) {
    status <- "unknown"
    status <- tryCatch(
      invisible(lapply(
        paste0("package:", pkgs_to_unload), unloadNamespace,
        character.only = TRUE, force = force)),
      warning = function(c) {
        msg <- conditionMessage(c)
        message(c)
        return("warning")
      },
      error = function(c) {
        msg <- conditionMessage(c)
        message(c)
        return("error")
      })
    if (!status %in% c("warning", "error")) {
      message("The following package(s) were detached: ",
              paste0(pkgs_to_unload, collapse = ", "))
    }
  } else {
    message("No packages were detached.")
  }
}
