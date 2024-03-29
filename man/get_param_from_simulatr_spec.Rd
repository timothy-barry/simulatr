% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parameter_accessors.R
\name{get_param_from_simulatr_spec}
\alias{get_param_from_simulatr_spec}
\title{Get parameter from simulatr_specifier object}
\usage{
get_param_from_simulatr_spec(simulatr_spec, row_idx, param)
}
\arguments{
\item{simulatr_spec}{a simulatr_specifier object}

\item{row_idx}{index giving the row of the parameter grid; if NULL, returns all values (if parameter stored in grid) or fixed value (if parameter stored in fixed params list)}

\item{param}{name of the parameter}
}
\value{
the value of the requested parameter
}
\description{
Returns the requested parameter from a simulatr_specifier object.
}
\details{
If the parameter is present in both the parameter grid and the fixed parameter list, returns value from parameter grid. If parameter is not present in either, throws an error.
}
