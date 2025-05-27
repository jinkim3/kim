#' Update the package 'kim'
#'
#' (Rewritten by ChatGPT on May 27, 2025.
#' May not function properly because of the rewrite.)
#'
#' Updates the current package 'kim' by installing the
#' most recent version of the package from GitHub.
#' This function requires the 'remotes' package.
#'
#' @param force logical. If TRUE, force installing the update.
#' By default, TRUE.
#' @param upgrade_other_pkg passed to `remotes::install_github`.
#' By default, FALSE.
#' @param confirm logical. If TRUE, ask the user before updating.
#' By default, TRUE.
#' @return Invisibly returns NULL. Called for side effects.
#' @export
update_kim <- function(
    force = TRUE,
    upgrade_other_pkg = FALSE,
    confirm = TRUE
) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    stop("The 'remotes' package is required but not installed. Please install it with install.packages('remotes').")
  }

  is_github_reachable <- function() {
    test_url <- "https://raw.githubusercontent.com/jinkim3/kim/master/DESCRIPTION"
    tryCatch({
      suppressWarnings(readLines(test_url, n = 1))
      TRUE
    }, error = function(e) FALSE)
  }

  safe_install_kim <- function() {
    tryCatch(
      remotes::install_github(
        "jinkim3/kim",
        force = force,
        upgrade = upgrade_other_pkg
      ),
      error = function(e) {
        message("\nFailed to update 'kim' from GitHub. This may be due to:")
        message("- No or expired GitHub token")
        message("- Network issues (e.g., firewall, VPN, proxy)")
        message("- GitHub API rate limits")
        message("You can try again later, or run:")
        message('  usethis::create_github_token()  # then')
        message('  gitcreds::gitcreds_set()        # to store it')
        return(invisible(NULL))
      }
    )
  }

  unload_kim_package <- function() {
    if ("package:kim" %in% search()) {
      try(detach("package:kim", unload = TRUE, character.only = TRUE), silent = TRUE)
    }
  }

  if (force) {
    unload_kim_package()
    if (is_github_reachable()) {
      safe_install_kim()
      try(kim::prep("kim", silent_if_successful = TRUE), silent = TRUE)
    } else {
      message("GitHub is not reachable. Skipping update.")
    }
  } else {
    current_pkg_version <- tryCatch(
      as.character(utils::packageVersion("kim")),
      error = function(e) "unknown"
    )

    github_url <- "https://raw.githubusercontent.com/jinkim3/kim/master/DESCRIPTION"
    github_pkg_desc <- tryCatch(
      readLines(github_url),
      warning = function(w) "github_desc_read_fail",
      error = function(e) "github_desc_read_fail"
    )

    if (!identical(github_pkg_desc, "github_desc_read_fail")) {
      github_pkg_version <- sub("^Version:\\s*", "", grep(
        "^Version:", github_pkg_desc, value = TRUE))
    } else {
      github_pkg_version <- "unknown"
    }

    compare_version_result <- tryCatch(
      utils::compareVersion(current_pkg_version, github_pkg_version),
      warning = function(w) 999,
      error = function(e) 999
    )

    if (current_pkg_version != "unknown" &&
        github_pkg_version != "unknown" &&
        compare_version_result == 0) {
      message(paste0(
        "Current version of 'kim': v", current_pkg_version,
        " (same as the most recent version available through GitHub)."
      ))
    } else if (current_pkg_version != "unknown" &&
               github_pkg_version != "unknown" &&
               compare_version_result > 0) {
      message(paste0(
        "Current version of 'kim': v", current_pkg_version,
        " (probably the most recent version available through GitHub)."
      ))
    } else {
      user_reply <- if (confirm && interactive()) {
        utils::menu(
          c("Yes.", "No."),
          title = "\nDo you want to try to update the package 'kim'?"
        )
      } else 1

      if (user_reply == 1) {
        unload_kim_package()
        if (is_github_reachable()) {
          safe_install_kim()
          try(kim::prep("kim", silent_if_successful = TRUE), silent = TRUE)
        } else {
          message("GitHub is not reachable. Skipping update.")
        }
      }
    }
  }

  invisible(NULL)
}
