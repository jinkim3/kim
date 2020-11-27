#' Update 'kim' package
#'
#' Updates the current package 'kim' by installing the most recent version
#'
#' @param source location of the most recent version of the package
#' (default = "github")
#'
#' @examples
#' \donttest{
#' update_kim()
#' }
#'
#' @export
#' @import devtools
update_kim <- function(source = "github") {
  if (source == "github") {
    # if source is github
    kim::prep("devtools")
    devtools::install_github("jinkim3/kim")
  }
}
