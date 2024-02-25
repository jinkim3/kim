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
#' assign_fn_parameters_as_vars(sum)
#' assign_fn_parameters_as_vars(sd)
#' assign_fn_parameters_as_vars(lm)
#' assign_fn_parameters_as_vars(floodlight_2_by_continuous)
#' }
#' @export
#' @import data.table
assign_fn_parameters_as_vars <- function(
  fun = NULL) {
  # function as string
  function_as_string <- deparse(substitute(fun))
  # message("77")
  # print(function_as_string)
  # search the function within the global environment
  if (exists(function_as_string, where = .GlobalEnv, inherits = FALSE)) {
    # print(1)
    kim::pm(
      "The function `", function_as_string, "` was found ",
      "within the global environment.")
  } else if (exists(
    function_as_string, where = baseenv(), inherits = FALSE)) {
    # search the function within the base environment
    # print(2)
    parameters <- formals(args(match.fun(function_as_string)))
    pkg_containing_the_function <- "base"
    kim::pm(
      "The function `", function_as_string, "` was found ",
      "within the base environment.")
  } else {
    # search the function within the attached packages
    # loop through the search path for attached packages and namespaces
    # print(3)
    # print(search())
    pkg_containing_the_function <- NULL
    for (env in search()) {
      if (exists(
        function_as_string, where = as.environment(env),
        inherits = FALSE)) {
        pkg_containing_the_function <- env
        # function_from_pkg_other_than_kim <- utils::getFromNamespace(
        #   function_as_string, pkg_containing_the_function)
        # parameters <- formals(args(match.fun(
        #   function_from_pkg_other_than_kim)))
        parameters <- formals(args(match.fun(function_as_string)))
        kim::pm(
          "The function `", function_as_string, "` was found ",
          "within Package '", pkg_containing_the_function, "'.")
        break
      }
    }
  }
  # search the function within the package kim
  if (is.null(pkg_containing_the_function)) {
    if (exists(
      function_as_string, where = asNamespace("kim"), inherits = FALSE)) {
      # print(4)
      function_from_kim <- utils::getFromNamespace(
        function_as_string, "kim")
      parameters <- formals(args(match.fun(function_from_kim)))
      kim::pm(
        "The function `", function_as_string, "` was found ",
        "within Package 'kim'.")
    }
  }
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
