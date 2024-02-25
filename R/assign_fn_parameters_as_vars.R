#' Assign function parameters as variables
#'
#' Take a function and assign all the parameters defined within it
#' as variables in the global environment
#'
#' This function can be useful when you are testing a function and
#' you need to set all the function's parameters in a single operation.
#'
#' @param fun a function
#' @examples
#' \dontrun{
#' assign_fn_parameters_as_vars(pm)
#' assign_fn_parameters_as_vars(mean)
#' assign_fn_parameters_as_vars(sd)
#' kim::assign_fn_parameters_as_vars(floodlight_2_by_continuous)
#' ?get
#' assign_fn_parameters_as_vars(scatterplot)
#' traceback()
#' exists("1")
#' ?exists("mean")
#' getAnywhere("mean")
#' environment(mean)
#' rm(list = ls())
#' function_as_string <- "floodlight_2_by_continuous"
#' search()
#' print(paste0("ls list: ", ls()))
#' }
#' @export
#' @import data.table
assign_fn_parameters_as_vars <- function(
  fun = NULL) {
  # function as string
  function_as_string <- deparse(substitute(fun))
  print(paste0("ls list: ", ls()))
  print(function_as_string)
  message("66")
  # search the function within the global environment
  if (exists(function_as_string, where = .GlobalEnv, inherits = FALSE)) {
    print(1)
  } else if (exists(
    function_as_string, where = baseenv(), inherits = FALSE)) {
    print(2)
  } else {
    print(3)
    # loop through the search path for attached packages and namespaces
    pkg_containing_the_function <- NULL
    for (env in search()) {
      if (exists(
        function_as_string, where = as.environment(env),
        inherits = FALSE)) {
        pkg_containing_the_function <- env
        print(pkg_containing_the_function)
        break
      }
    }
    print(pkg_containing_the_function)
  }
  # else if (exists(
  #   function_as_string, where = asNamespace("kim"), inherits = FALSE)) {
  #   # search the function within the package kim
  #   print(4)
  #   function_from_kim <- utils::getFromNamespace(
  #     function_as_string, "kim")
  #   parameters <- formals(args(match.fun(function_from_kim)))
  #   kim::pm(
  #     "The function `", function_as_string, "` was found ",
  #     "within Package 'kim'.")
  # }
  # manually assign each element of the list to the global environment
  for (name in names(parameters)) {
    assign(name, parameters[[name]], envir = .GlobalEnv)
  }
  # notify the user of the assignments
  kim::pm(
    "Parameters of the function `", function_as_string,
    "` now should have been assigned\nas variables in ",
    "the global environment.")
}
