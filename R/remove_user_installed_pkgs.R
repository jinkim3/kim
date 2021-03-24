#' Remove all user installed packages
#'
#' @param exceptions a character vector of names of packages to keep
#' @param type_of_pkg_to_keep a character vector indicating types
#' of packages to keep. The default,
#' \code{type_of_pkg_to_keep = c("base", "recommended")}, keeps all
#' base and recommended packages that come with R when R is installed.
#' @param keep_kim logical. If \code{keep_kim = FALSE}, Package 'kim'
#' will be removed along with all other user-installed packages.
#' If \code{keep_kim = TRUE}, Package 'kim' will not be removed.
#' By default, \code{keep_kim = FALSE}
#' @examples
#' \dontrun{
#' remove_user_installed_pkgs()
#' }
#' @export
remove_user_installed_pkgs <- function(
  exceptions = NULL,
  type_of_pkg_to_keep = c("base", "recommended"),
  keep_kim = FALSE
) {
  # get information on installed packages
  pkg_df <- as.data.frame(utils::installed.packages())
  # names of all packages
  all_pkg <- pkg_df[["Package"]]
  # default packages to keep (base or recommended)
  default_pkg <- subset(
    pkg_df, Priority %in% c("base", "recommended"))[["Package"]]
  # keep kim?
  if (keep_kim == TRUE) {
    exceptions <- c(exceptions, "kim")
  }
  # add exceptions
  if (!is.null(exceptions)) {
    # if the exception packages actually do not exist to begin with
    nonexistent_user_installed_pkg <- setdiff(exceptions, all_pkg)
    if (length(nonexistent_user_installed_pkg) > 0) {
      message(paste0(
        "The following package(s) have not been installed to begin with:\n",
        paste0(nonexistent_user_installed_pkg, collapse = ", ")))
    }
    # check if exception packages do exist
    exceptions <- intersect(exceptions, all_pkg)
    if (length(exceptions) > 0) {
      message(paste0(
        "The following user-installed package(s) will be kept:\n",
        paste0(exceptions, collapse = ", "), "\n"))
    }
  }
  # which packages to keep?
  pkg_to_keep <- c(default_pkg, exceptions)
  # which packages to remove
  pkg_to_remove <- setdiff(all_pkg, pkg_to_keep)
  # ask the user to confirm the removal
  if (length(pkg_to_remove) > 0) {
    # list the packages to remove
    message(paste0(
      "A total of ", length(pkg_to_remove),
      " packages to remove:\n\n",
      paste0(pkg_to_remove, collapse = ", "), "\n"))
    user_reply_1 <- menu(
      c("Yes, remove all.", "No, do not remove them."),
      title = paste0(
        "Do you really want to remove all of the above ",
        length(pkg_to_remove),
        " packages?"))
    # ask the user again to confirm the removal
    if (user_reply_1 == 1) {
      user_reply_2 <- menu(
        c("Yes, I am sure. Remove all of the above packages.",
          "No, do not remove any package(s)."),
        title = paste0(
          "You are about to remove all of the above ",
          length(pkg_to_remove),
          " packages. Are you sure?"))
      if (user_reply_2 == 1) {
        # path of the packages to remove
        path_of_pkg_to_remove <- subset(
          pkg_df, Package %in% pkg_to_remove)[["LibPath"]]
        # if there is only one unique path
        if (length(unique(path_of_pkg_to_remove)) == 1) {
          utils::remove.packages(
            pkgs = pkg_to_remove,
            lib = unique(path_of_pkg_to_remove))
        } else {
          for (i in seq_along(pkg_to_remove)) {
            utils::remove.packages(
              pkg_to_remove[i], path_of_pkg_to_remove[i])
          }
        }
      } else {
        message("No package(s) were removed.")
      }
    } else {
      message("No package(s) were removed.")
    }
  } else {
    message("There were no package(s) to remove.")
  }
}
