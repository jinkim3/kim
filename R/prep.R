#' Prepare package(s) for use
#'
#' Installs, loads, and attaches package(s). If package(s) are not
#' installed, installs them prior to loading and attaching.
#'
#' @param ... names of packages to load and attach, separated by commas,
#' e.g., \code{"ggplot2", data.table}. The arguments can be any number
#' of packages, and they may or may not be wrapped in quotes.
#'
#' @examples
#' \donttest{
#' prep(data.table)
#' prep("base", utils, ggplot2, "data.table")
#' }
#'
#' @export
prep <- function(...) {
  # which packages were entered?
  entered_pkg <- utils::tail(
    as.character(match.call(expand.dots = TRUE)), -1
  )
  # list of currently installed packages
  installed_packages_1 <- rownames(utils::installed.packages())
  # which packages need to be installed?
  pkg_to_install <- entered_pkg[
    which(!entered_pkg %in% installed_packages_1)
  ]
  if (length(pkg_to_install) > 0) {
    # install packages
    utils::install.packages(pkg_to_install)
  }
  # line break
  cat("\n")
  # list of installed packages after installing entered packages
  installed_packages_2 <- rownames(utils::installed.packages())
  # newly installed packages
  newly_installed_pkg <- setdiff(
    installed_packages_2, installed_packages_1
  )
  # packages that failed to install
  pkg_that_failed_to_install <- setdiff(
    pkg_to_install, installed_packages_2
  )
  # check which packages can be loaded (and attached)
  pkg_to_check_if_loaded <- entered_pkg[
    which(entered_pkg %in% installed_packages_2)
  ]
  if (length(pkg_to_check_if_loaded) > 0) {
    # currently loaded packages
    pkg_loaded <- .packages()
    pkg_to_load <- pkg_to_check_if_loaded[
      which(!pkg_to_check_if_loaded %in% pkg_loaded)
    ]
    if (length(pkg_to_load) > 0) {
      # load and attach packages
      invisible(lapply(pkg_to_load, library, character.only = TRUE))
    }
  }
  # packages that failed to install
  if (length(pkg_that_failed_to_install) > 0) {
    message(paste0(
      "The following package(s) failed to install:\n",
      paste(pkg_that_failed_to_install, collapse = ", "), "\n"
    ))
  }
  # packages that failed to load
  pkg_that_failed_to_load <- setdiff(
    entered_pkg, rownames(utils::installed.packages())
  )
  if (length(pkg_that_failed_to_load) > 0) {
    message(paste0(
      "The following package(s) failed to load:\n",
      paste(pkg_that_failed_to_load, collapse = ", "), "\n"
    ))
  }
  # newly installed packages
  if (length(newly_installed_pkg) > 0) {
    message(paste0(
      "The following package(s) were newly installed:\n",
      paste(newly_installed_pkg, collapse = ", "), "\n"
    ))
  }
  # packages that were loaded
  loaded_pkg_final <- intersect(
    entered_pkg, rownames(utils::installed.packages())
  )
  if (length(loaded_pkg_final) > 0) {
    message(paste0(
      "The following package(s) were successfully loaded:\n",
      paste(loaded_pkg_final, collapse = ", "), "\n"
    ))
  }
}
