% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_functs.R
\name{compute_bias}
\alias{compute_bias}
\title{Compute bias}
\usage{
compute_bias(tbl, key, sim_spec)
}
\arguments{
\item{tbl}{data frame with columns target and value. Target should have entry "estimate."}

\item{key}{data frame with columns for parameter and grid_row_id}

\item{sim_spec}{a simulatr specifier object}
}
\value{
a 1-row tibble with columns value, lower_mc_ci, and upper_mc_ci
}
\description{
Computes the bias of an estimator.
}
