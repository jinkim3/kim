#' Prepare package(s) for use
#'
#' Installs, loads, and attaches package(s). If package(s) are not
#' installed, installs them prior to loading and attaching.
#'
#' @param ... names of packages to load and attach, separated by commas,
#' e.g., \code{"ggplot2", data.table}. The input can be any number
#' of packages, whose names may or may not be wrapped in quotes.
#' @param pkg_names_as_object logical. If \code{pkg_names_as_object = TRUE},
#' the input will be evaluated as one object containing package names.
#' If \code{pkg_names_as_object = FALSE}, the input will be considered
#' as literal packages names (default = FALSE).
#' @param silent_if_successful logical. If \code{silent_if_successful = TRUE},
#' no message will be printed if preparation of package(s) is successful.
#' If \code{silent_if_successful = FALSE}, a message indicating which
#' package(s) were successfully loaded and attached will be printed
#' (default = FALSE).
#' @param silent_load_pkgs a character vector indicating names of
#' packages to load silently (i.e., suppress messages that get printed
#' when loading the packaged). By default, \code{silent_load_pkgs = NULL}
#' @return there will be no output from this function. Rather, packages
#' given as inputs to the function will be installed, loaded, and attached.
#' @examples
#' \donttest{
#' prep(data.table)
#' prep("data.table", silent_if_successful = TRUE)
#' prep("base", utils, ggplot2, "data.table")
#' pkgs <- c("ggplot2", "data.table")
#' prep(pkgs, pkg_names_as_object = TRUE)
#' prep("data.table", silent_load_pkgs = "data.table")
#' }
#'
#' @export
prep <- function(
  ...,
  pkg_names_as_object = FALSE,
  silent_if_successful = FALSE,
  silent_load_pkgs = NULL) {
  # list of packages entered
  arg_list <- as.list(match.call(expand.dots = FALSE))[["..."]]
  # if "... = x" was entered
  if ("..." %in% names(arg_list)) {
    arg_list <- arg_list[["..."]]
  }
  # if arg_list at this point is language, evaluate it
  if (typeof(arg_list) == "language") {
    arg_list <- eval(arg_list)
  }
  # names of packages
  # check whether an object containing package names was entered
  # (e.g., a vector of package names)
  if (pkg_names_as_object == TRUE) {
    if (length(arg_list) != 1) {
      stop(paste0(
        "If pkg_names_as_object = TRUE, exactly one object containing ",
        "package names can be entered.\n",
        "You have entered ", length(arg_list), " object(s)."))
    } else {
      pkg_names <- eval.parent(arg_list[[1]])
    }
  } else {
    pkg_names <- lapply(arg_list, as.character)
  }
  if (length(pkg_names) < 1) {
    stop("Please enter name(s) of package(s) to prepare for use")
  }
  # unlist package names
  entered_pkg <- unlist(pkg_names)
  # done dealing with inputs

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
      # silently load packages
      if (length(silent_load_pkgs) > 0) {
        # split pkg_to_load into two groups: (1) packages to be loaded
        # silently and (2) packages to be loaded not silently
        pkg_to_actually_load_silently <- intersect(
          silent_load_pkgs, pkg_to_load)
        pkg_to_load_not_silently <- setdiff(
          pkg_to_load, silent_load_pkgs)
        # is any of the packages to be loaded silently in pkg_to_load?
        if (length(pkg_to_actually_load_silently) > 0) {
          invisible(lapply(pkg_to_actually_load_silently, function(x) {
            suppressMessages(library(x, character.only = TRUE))
          }))
          if (length(pkg_to_load_not_silently) > 0) {
            # load and attach packages
            invisible(lapply(
              pkg_to_load_not_silently, library, character.only = TRUE))
          }
        } else {
          stop(paste0(
            "None of the packages given for the argument silent_load_pkgs ",
            "is in the packages given for the argument pkg_to_load."))
        }
      } else {
        # load and attach packages
        invisible(lapply(pkg_to_load, library, character.only = TRUE))
      }
    }
  }
  # packages that failed to install
  if (length(pkg_that_failed_to_install) > 0) {
    message(paste0(
      "\nThe following package(s) failed to install:\n",
      paste(pkg_that_failed_to_install, collapse = ", "), "\n"
    ))
  }
  # packages that failed to load
  pkg_that_failed_to_load <- setdiff(
    entered_pkg, rownames(utils::installed.packages())
  )
  if (length(pkg_that_failed_to_load) > 0) {
    message(paste0(
      "\nThe following package(s) failed to load:\n",
      paste(pkg_that_failed_to_load, collapse = ", "), "\n"
    ))
  }
  # newly installed packages
  if (length(newly_installed_pkg) > 0) {
    message(paste0(
      "\nThe following package(s) were newly installed:\n",
      paste(newly_installed_pkg, collapse = ", "), "\n"
    ))
  }
  # packages that were loaded
  loaded_pkg_final <- intersect(
    entered_pkg, rownames(utils::installed.packages())
  )
  if (length(loaded_pkg_final) > 0) {
    if (silent_if_successful == FALSE) {
      message(paste0(
        "\nThe following package(s) were successfully loaded:\n",
        paste(loaded_pkg_final, collapse = ", "), "\n"
      ))
    }
  }
}
