# display the current version
.onAttach <- function(libname, pkgname) {
  # notify the option to update? (5 possible cases)
  # 1. error in getting the current package version -> yes
  # 2. error in getting the github package version -> no
  #    -> this used to be "yes," but as of aug 22, 2023, it's now "no"
  # 3. current package version < github package version -> yes
  # 4. current package version > github package version -> no
  # 5. current package version == github package version -> no
  # in short, notify the option to update unless the version numbers match
  # get version of the currently installed package
  current_pkg_version <- tryCatch(
    as.character(utils::packageVersion("kim")),
    error = function(e) "unknown")
  # github url
  github_url <- paste0(
    "https://raw.githubusercontent.com/jinkim3/",
    "kim/master/DESCRIPTION")
  # get github description or handle errors
  github_pkg_desc <- tryCatch(
    readLines(github_url),
    warning = function(w) {"github_desc_read_fail"},
    error = function(e) {"github_desc_read_fail"})
  # get the version number of github version
  if (identical(github_pkg_desc, "github_desc_read_fail")) {
    # case 2. error in getting the github package version -> no
    github_pkg_version <- "unknown"
    startup_message <- paste0(
      "Package attached: kim v", current_pkg_version,
      "\nThe function failed to learn about the most recent version",
      " available on\nGitHub (possbly due to the lack of ",
      "Internet connection).",
      "\n\nYou have an option to update the package ",
      "with the\nfunction `update_kim()`. ",
      "If you do so, it is recommended that you restart R.\n")
  } else {
    # cases 1, 3-5.
    github_pkg_version <- gsub(
      ".*ersion: ", "", github_pkg_desc[
        grep("ersion", github_pkg_desc)][1])
    # compare versions
    compare_version_result <- tryCatch(
      utils::compareVersion(
        current_pkg_version, github_pkg_version),
      warning = function(w) {999}, # 999 indicates no need for update
      error = function(e) {999})
    # case 5. current package version == github package version -> no
    # skip update for case 5
    if (current_pkg_version != "unknown" &
        github_pkg_version != "unknown" &
        compare_version_result == 0) {
      startup_message <- paste0(
        "Package attached: kim v", current_pkg_version,
        " (same as the most recent version available through GitHub).")
    } else if (
      # case 4. current package version > github package version -> no
      # skip update for case 4
      current_pkg_version != "unknown" &
      github_pkg_version != "unknown" &
      compare_version_result > 0) {
      startup_message <- paste0(
        "Package attached: kim v", current_pkg_version,
        " (probably the most recent version available through GitHub).")
    } else {
      # cases. 1 and 3.
      # simply notify of the OPTION to update the package
      # this is simply a notification of the option to update,
      # rather than a recommendation to update
      startup_message <- paste0(
        "Package attached: kim v", current_pkg_version,
        "; Most recent version available on GitHub: v",
        github_pkg_version,
        "\n\nYou have an option to update the package ",
        "with the\nfunction `update_kim()`. ",
        "If you do so, it is recommended that you restart R.\n")
    }
  }
  packageStartupMessage(startup_message)
}
