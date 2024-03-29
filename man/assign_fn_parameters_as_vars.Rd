% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assign_fn_parameters_as_vars.R
\name{assign_fn_parameters_as_vars}
\alias{assign_fn_parameters_as_vars}
\title{Assign function parameters as values}
\usage{
assign_fn_parameters_as_vars(fun = NULL, envir = NULL)
}
\arguments{
\item{fun}{a function}

\item{envir}{an environment in which to assign the parameters as
values (default = \code{.GlobalEnv})}
}
\description{
Take a function and assign all the parameters defined within it
as values in the specified environment (e.g., global environment)
}
\details{
This function can be useful when you are testing a function and
you need to set all the function's parameters in a single operation.
}
\examples{
\dontrun{
assign_fn_parameters_as_vars(pm)
assign_fn_parameters_as_vars(mean)
assign_fn_parameters_as_vars(sum)
assign_fn_parameters_as_vars(lm)
assign_fn_parameters_as_vars(floodlight_2_by_continuous)
}
}
