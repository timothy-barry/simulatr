simulatr_function <- setClass("simulatr_function",
                              slots = list(f = "function",
                                           arg_names = "character",
                                           packages = "character",
                                           loop = "logical"))

check_validity_simulatr_specifier <- function(object) {
  all_params <- union(colnames(object@parameter_grid), names(object@fixed_parameters))
  key_params_included <- sapply(c("B", "seed", "n_processors"), function(i) i %in% all_params)
  if (all(key_params_included)) {
    out <- TRUE
  } else {
    not_included_params <- names(which(!key_params_included))
    out <- paste0("The following required parameter(s) were not found in the simulatr specifier object: ",
                  paste0(not_included_params, collapse = ", "),
                  ". Add these parameter(s) to the simulatr specifier object as columns in the parameter grid or as values in the fixed parameters list.")
  }
  return(out)
}

simulatr_specifier <- setClass("simulatr_specifier",
                               slots = list(parameter_grid = "data.frame",
                                            fixed_parameters = "list",
                                            generate_data_function = "simulatr_function",
                                            run_method_functions = "list"),
                               validity = check_validity_simulatr_specifier)
