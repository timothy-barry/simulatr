% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_functs.R
\name{summarize_results}
\alias{summarize_results}
\title{Summarize results}
\usage{
summarize_results(sim_spec, sim_res, metrics, parameters, threshold = 0.05)
}
\arguments{
\item{sim_spec}{a \code{simulatr_specifier} object}

\item{sim_res}{the data frame of raw results, as outputted by \code{simulatr}}

\item{metrics}{character vector of metrics to compute; options include "coverage," "bias," "se" (standard deviation of estimator), "mse", "rejection_probability," "count," and "time."}

\item{parameters}{character vector of parameters on which to compute the metrics}

\item{threshold}{(optional; default value 0.05) the rejection threshold to use for "rejection_probability" metric.}
}
\value{
a data frame of summarized results
}
\description{
Summarizes the results of a \code{simulatr} simulation.
}
