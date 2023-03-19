#' Setup script
#'
#' Runs setup for either the generate_data.R script or run_methods.R script. This script
#'  i) sets the seed (which has a global side-effect)
#'  ii) updates the value of B, if B !=0 (the default)
#'  iii) obtains the ordered set of arguments
#'
#' @param simulatr_spec simulation specifier object
#' @param B_in B to set (0 means do not update)
#' @param function_object either a method or data generation simulatr_function object
#' @param row_idx row index of the parameter grid
#'
#' @return a list with the B-updated sim_spec object and ordered list of args
#' @export
setup_script <- function(simulatr_spec, B_in, function_object, row_idx) {
  # 1. set the seed
  seed <- get_param_from_simulatr_spec(simulatr_spec, row_idx, "seed")
  set.seed(seed) # side effect

  # 2. set B to B_in if B_in is nonzero
  if (B_in != 0) simulatr_spec <- update_B_sim_spec(simulatr_spec, B_in)

  # 3. obtain arguments
  ordered_args <-
    if (identical(function_object@arg_names, NA_character_)) {
      list()
    } else {
      lapply(function_object@arg_names, function(curr_arg)
        get_param_from_simulatr_spec(simulatr_spec, row_idx, curr_arg))
    }
  return(list(simulatr_spec = simulatr_spec, ordered_args = ordered_args))
}


#' Collate result list
#'
#' @param result_df data frame outputted by method function
#' @param proc_id the processor id
#' @param row_idx the row of the parameter grid
#' @param method string giving the method
#'
#' @return a collated data frame
#' @export
collate_result_list <- function(result_df, proc_id, row_idx, method) {
  colnames_result_df <- colnames(result_df)
  convert_to_factor <- c(c("method"),
                         if ("parameter" %in% colnames_result_df) "parameter" else NULL,
                         if ("target" %in% colnames_result_df) "target" else NULL)

  out <- result_df %>% dplyr::mutate(proc_id = proc_id,
                              grid_id = row_idx,
                              method = method,
                              id = paste0(method, "-",
                                          row_idx, "-",
                                          proc_id, "-",
                                          as.character(run_id))) %>%
    dplyr::mutate_at(convert_to_factor, factor)
  return(out)
}