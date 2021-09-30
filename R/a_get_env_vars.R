#' Get Environment Variables
#'
#' @examples
#' a_get_env_vars()
#' @export
a_get_env_vars <- function() {
  if (!Sys.getenv()[["COMPUTERNAME"]] == "CRANWIN" &
      !grepl("[Ll](?i)(igges)", Sys.getenv()[["CLIENTNAME"]])) {
    print("this is cra")
  } else {
    print("not sure if this is cra")
  }
}
