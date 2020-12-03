# message to print when loading the package
.onAttach <- function(libname, pkgname) {
  # get current version
  installed_pkg_version <- tryCatch(
    as.character(utils::packageVersion("kim")), error = function(e) NA)
  # github url
  url <- "https://raw.githubusercontent.com/jinkim3/kim/master/DESCRIPTION"
  # get github description or handle errors
  github_pkg_desc <- tryCatch(
    readLines(url),
    warning = function(w) {"no_result"},
    error = function(e) {"no_result"})
  if (identical(github_pkg_desc, "no_result")) {
    if (is.na(installed_pkg_version)) {
      startup_message <- paste0(
        "Attached: kim (version unknown)\nThis may not be the",
        " most recent version of the package available on github.",
        "\nIf you run into errors using the package, please consider",
        " updating the package by running 'update_kim()'")
    } else {
      startup_message <- paste0(
        "Attached: kim ", installed_pkg_version, "\nThis may not be the",
        " most recent version available on github.",
        "\nIf you run into errors using the package, please consider",
        " updating the package by running 'update_kim()'")
    }
  } else {
    # get the version number of github version
    github_pkg_version <- gsub(
      ".*ersion: ", "", github_pkg_desc[grep("ersion", github_pkg_desc)])
    # compare versions
    compare_version_result <- tryCatch(
      utils::compareVersion(installed_pkg_version, github_pkg_version),
      warning = function(w) {NA},
      error = function(e) {NA})
    # message to print
    if (is.na(compare_version_result)) {
      startup_message <- paste0(
        "Attached: kim (version unknown)\nThis may not be the",
        " most recent version of the package available on github.",
        "\nIf you run into errors using the package, please consider",
        " updating the package by running 'update_kim()'")
    } else if (compare_version_result == -1) {
      startup_message <- paste0(
        "\nA more recent version of the package 'kim' is available ",
        "on github:\n",
        "Currently installed version: ", installed_pkg_version,"\n",
        "Available newer version:     ", github_pkg_version,
        "\nIf you run into errors using the package, please ",
        "consider updating by running 'update_kim()'")
    } else if (compare_version_result == 0) {
      startup_message <- paste0(
        "Attached: kim ", installed_pkg_version, "(the most recent version ",
        "available on github")
    } else if (compare_version_result == 1) {
      startup_message <- paste0(
        "You are running a newer, developer version (v",
        installed_pkg_version, ") of the ",
        "package 'kim' that may not be stable.\n",
        "If you run into errors using the package, please consider ",
        "downgrading the package by running 'update_kim()'"
      )
    }
  }
  packageStartupMessage(startup_message)
}
