#' Install all dependencies for all functions
#'
#' Install all dependencies for all functions in Package 'kim'.
#'
#' @return there will be no output from this function. Rather,
#' dependencies of all functions in Package 'kim' will be installed.
#' @examples
#' \dontrun{
#' install_all_dependencies()
#' }
#' @export
install_all_dependencies <- function() {
  # packages and dependencies to install
  pkgs <- c(
    "boot", "car", "data.table", "effsize", "ggplot2",
    "ggridges", "gridExtra", "interactions", "lemon", "lm.beta",
    "mediation", "moments", "paran", "psych", "remotes", "weights",
    "WRS2", "MASS")
  # prep the packages
  kim::prep(pkgs, pkg_names_as_object = TRUE)
}
