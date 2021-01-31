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
#' @examples
#' \dontrun{
#' start_kim(update = T, setup_r_env = F)
#' }
#' @export
start_kim <- function(
  update = TRUE,
  setup_r_env = TRUE,
  default_packages = c("data.table", "ggplot2")) {
  # update the package
  if (update == TRUE) {
    kim::update_kim()
  }
  # set up r env
  if (setup_r_env == TRUE) {
    kim::setup_r_env()
  }
  # default packages
  if (length(default_packages) > 0) {
    # which packages were entered?
    entered_pkg <- default_packages
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
}
