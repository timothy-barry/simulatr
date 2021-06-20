#' Get parameter from simulatr_specifier object
#'
#' Returns the requested parameter from a simulatr_specifier object.
#'
#' If the parameter is present in both the parameter grid and the fixed parameter list, returns value from parameter grid. If parameter is not present in either, throws an error.
#'
#' @param simulatr_spec a simulatr_specifier object
#' @param row_idx index giving the row of the parameter grid; if NULL, returns all values (if parameter stored in grid) or fixed value (if parameter stored in fixed params list)
#' @param param name of the parameter
#'
#' @return the value of the requested parameter
#'
#' @export
get_param_from_simulatr_spec <- function(simulatr_spec, row_idx, param) {
  param_grid <- simulatr_spec@parameter_grid
  fixed_params <- simulatr_spec@fixed_parameters
  if (param %in% colnames(param_grid)) {
    if (is.null(row_idx)) param_grid[, param] else param_grid[row_idx, param]
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
#' @param B_in value of B passed to command line; B = 0 indicates no argument passed
#'
#' @return NULL
#' @export
get_params_for_nextflow <- function(simulatr_spec, fp, B_in) {
  # Basic quantities
  method_names <- names(simulatr_spec@run_method_functions)
  n_processors <- get_param_from_simulatr_spec(simulatr_spec, NULL, "n_processors")
  n_param_settings <- nrow(simulatr_spec@parameter_grid)
  # B
  B <- if (B_in == 0) get_param_from_simulatr_spec(simulatr_spec, NULL, "B") else B_in
  # wall time calculation function
  get_wall_time <- function(funct_object, n_processors, B) {
    one_rep_time <- funct_object@one_rep_time
    mult_time_factor <- funct_object@mult_time_factor
    add_time_factor <- funct_object@add_time_factor
    if (is.na(one_rep_time) | is.na(mult_time_factor) | is.na(add_time_factor)) {
      out <- 0
    } else {
      out <- (mult_time_factor * B * one_rep_time)/n_processors + add_time_factor
      out <- ceiling(out)
    }
    return(out)
  }
  method_times <- lapply(simulatr_spec@run_method_functions, get_wall_time, n_processors = n_processors, B = B)
  data_generator_time <- get_wall_time(simulatr_spec@generate_data_function, 1, B)
  # Put everything into list
  to_write_list <- c(list(method_names = method_names,
                        n_param_settings = n_param_settings,
                        data_generator = data_generator_time),
                     method_times)

  # write to file
  rhs <- sapply(to_write_list, function(entry) paste0(entry, collapse = "-"))
  lhs <- names(to_write_list)
  lines <- paste0(lhs, ":", rhs)
  fcon <- file(fp)
  writeLines(lines, fcon)
  close(fcon)
}


#' Update B in a simulatr_specifier object
#'
#' Takes a simulatr_specifier object and a value for B; updates B and return a new object.
#'
#' @param simulatr_spec a simulatr specifier object
#' @param B the new value of B
#'
#' @return an updated simulatr_specifier object
#' @export
update_B_sim_spec <- function(simulatr_spec, B) {
  if ("B" %in% colnames(simulatr_spec@parameter_grid)) {
    simulatr_spec@parameter_grid$B <- B
  } else if ("B" %in% names(simulatr_spec@fixed_parameters)) {
    simulatr_spec@fixed_parameters[["B"]] <- B
  } else {
    stop("'B' is not present in the parameter grid or fixed parameter list in the simulatr specifier object.
         Add 'B' to either of these fields to access B from the command line.")
  }
  return(simulatr_spec)
}
