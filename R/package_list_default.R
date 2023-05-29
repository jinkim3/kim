#' Packages - List the default packages
#'
#' List the default packages in R
#'
#' @param package_type a vector of package types. By default,
#' \code{package_type = c("base", "recommended")}
#' @examples
#' package_list_default()
#' package_list_default(package_type = "base")
#' @export
package_list_default <- function(
    package_type = c("base", "recommended")) {
  # bind the var(s) locally to the function
  Priority <- NULL
  pkg_df <- as.data.frame(utils::installed.packages())
  pkg_list <- vector("list", length(package_type))
  for (i in seq_along(package_type)) {
    pkg_list[[i]] <- subset(
      pkg_df, Priority == package_type[i])[["Package"]]
  }
  names(pkg_list) <- package_type
  if (identical(sort(package_type), c("base", "recommended"))) {
    message(paste0(
      "Below is the list of default ",
      'R packages ("base" and "recommended").'))
  }
  return(pkg_list)
}
