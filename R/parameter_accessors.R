#' Get parameter from simulatr_specifier object
#'
#' Returns the requested parameter from a simulatr_specifier object.
#'
#' If the parameter is present in both the parameter grid and the fixed parameter list, returns value from parameter grid. If parameter is not present in either, throws an error.
#'
#' @param simulatr_spec a simulatr_specifier object
#' @param row_idx index giving the row of the parameter grid
#' @param param name of the parameter
#'
#' @return the value of the requested parameter
#'
#' @export
#' @examples
#' load_all()
#' simulatr_spec <- get_example_simulatr_specifier()
#' get_param_from_simulatr_spec(simulatr_spec, 10, "beta_1")
get_param_from_simulatr_spec <- function(simulatr_spec, row_idx, param) {
  param_grid <- simulatr_spec@parameter_grid
  fixed_params <- simulatr_spec@fixed_parameters
  if (param %in% colnames(param_grid)) {
    param_grid[row_idx, param]
  } else if (param %in% names(fixed_params)) {
    fixed_params[[param]]
  } else {
    stop(paste0("Parameter ", param, " not present in simulatr_specifier object."))
  }
}


#' Get params for nextflow
#'
#' Given a file path fp, writes all parameters to which nextflow needs access to a plain text file stored at fp.
#'
#' Parameters are separated by line. Each line is of the form "param:value," where "param" is the name of the parameter and "value" is its value.
#'
#' @param simulatr_spec the simulatr_specification object
#' @param fp a file path
#'
#' @return NULL
#' @export
get_params_for_nextflow <- function(simulatr_spec, fp) {
  # n param settings
  n_param_settings <- nrow(simulatr_spec@parameter_grid)
  # n cores
  n_cores <- simulatr_spec@fixed_parameters[["n_cores"]]
  # methods
  method_names <- paste0(names(simulatr_spec@run_method_functions), collapse = "-")
  # write to file
  to_write_list <- list("n_param_settings" = n_param_settings,
                   "n_cores" = n_cores,
                   "method_names" = method_names)
  lines <- sapply(seq(1, length(to_write_list)), function(i)
    paste0(names(to_write_list[i]), ":", to_write_list[i]))
  fcon <- file(fp)
  writeLines(lines, fcon)
  close(fcon)
  return(NULL)
}
