#' Check simulatr specifier object
#'
#' Checks a simulatr specifier object for errors or warnings.
#'
#' Note: possibly add timer and parallelization.
#'
#' @param simulatr_spec a simulatr_specifier object
#' @param B_in number of resamples to use
#'
#' @return if no errors, the list of results; if errors occur, the ordered list of arguments passed to the function in which the error occurred.
#' @export
#'
#' @examples
#' simulatr_spec <- readRDS("/Users/timbarry/research_offsite/glmeiv/private/simulations/sim_spec_1.rds")
#' simulatr_spec@fixed_parameters[["n"]] <- 500
#' check <- check_simulatr_specifier_object(simulatr_spec, 5)
check_simulatr_specifier_object <- function(simulatr_spec, B_in = NULL) {
  # set basic quantities
  n_param_settings <- nrow(simulatr_spec@parameter_grid)
  data_lists <- vector(mode = "list", length = n_param_settings)
  data_generation_times <- vector(mode = "numeric", length = n_param_settings)

  # update B in the fixed parameter list
  if (!is.null(B_in)) {
    simulatr_spec@fixed_parameters[["B"]] <- B_in
    if ("B" %in% colnames(simulatr_spec@parameter_grid)) simulatr_spec@parameter_grid$B <- NULL
  }

  # set up for data generation across parameter settings; load data generation packages (if necessary)
  data_generator <- simulatr_spec@generate_data_function
  packs_to_load <- data_generator@packages
  if (!(identical(packs_to_load, NA_character_))) invisible(lapply(packs_to_load, function(pack) library(pack, character.only = TRUE)))

  # Generate the synthetic data
  for (row_idx in seq(1, n_param_settings)) {
    print(paste0("Generating dataset ", row_idx, " of ", n_param_settings, "."))
    out <- tryCatch({
    # set seed for given row
    seed <- get_param_from_simulatr_spec(simulatr_spec, row_idx, "seed")
    set.seed(seed)
    # obtain arguments
    ordered_args <- lapply(data_generator@arg_names, function(curr_arg) {
      get_param_from_simulatr_spec(simulatr_spec, row_idx, curr_arg)
      })
    # obtain B; if B_in has been passed, use that instead
    B <- get_param_from_simulatr_spec(simulatr_spec, row_idx, "B")
    time <- system.time(
    if (data_generator@loop) {
      data_list <- replicate(B, do.call(data_generator@f, ordered_args), FALSE)
    } else {
      data_list <- do.call(data_generator@f, ordered_args)
    })[["elapsed"]]/B
    list(OK = TRUE, data_list = data_list, time = time)
    },
    error = function(e) {
      message(paste0("The data generation function failed for the parameter setting stored in row ", row_idx, " of the parameter grid. Aborting and returning the ordered list of arguments. The error was as follows:"))
      message(e)
      message("\n")
      return(list(OK = FALSE, ordered_args = ordered_args))
    },
    warning = function(w) {
      message(paste0("Data generation function yielded warning for parameter setting ", row_idx, ". Warning was as follows:"))
      message(w)
      message("\n")
      return(list(OK = TRUE, data_list = data_list, time = time))
    })
    if (out$OK) {
      data_lists[[row_idx]] <- out$data_list
      data_generation_times[row_idx] <- out$time
    } else {
      return(out$ordered_args)
    }
  }

  # next, apply each method to each simulated dataset in a double for loop
  method_names <- names(simulatr_spec@run_method_functions)
  n_methods <- length(method_names)
  result_lists <- method_times <- vector(mode = "list", length = n_methods)
  names(result_lists) <- names(method_times) <- method_names
  for (method in method_names) {
    method_times[[method]] <- vector(mode = "numeric", length = n_param_settings)
    result_lists[[method]] <- vector(mode = "list", length = n_param_settings)
  }

  for (method_name in method_names) {
    # load method-specific packages
    method_object <- simulatr_spec@run_method_functions[[method_name]]
    packs_to_load <- method_object@packages
    if (!(identical(packs_to_load, NA_character_))) invisible(lapply(packs_to_load, function(pack)
      library(pack, character.only = TRUE)))
    # run method across all parameter settings
    for (row_idx in seq(1, n_param_settings)) {
      print(paste0("Running method ", method_name, " on dataset ", row_idx, " of ", n_param_settings, "."))
      out <- tryCatch({
        # set the seed
        seed <- get_param_from_simulatr_spec(simulatr_spec, row_idx, "seed")
        set.seed(seed)
        # obtain arguments
        ordered_args <- c(list(NA), lapply(method_object@arg_names, function(curr_arg) {
          get_param_from_simulatr_spec(simulatr_spec, row_idx, curr_arg)
        }))
        data_list <- data_lists[[row_idx]]
        B <- length(data_list)
        time <- system.time(if (method_object@loop) {
          result_list <- lapply(seq(1, length(data_list)), function(i) {
            ordered_args[[1]] <- data_list[[i]]
            out <- do.call(method_object@f, ordered_args)
            out$run_id <- i
          })
          result_df <- do.call(rbind, result_list)
        } else {
          ordered_args[[1]] <- data_list
          result_df <- do.call(method_object@f, ordered_args)
        })[["elapsed"]]/B

        list(OK = TRUE, result_df = result_df, time = time)
      },
      error = function(e) {
        message(paste0("Method function \'", method_name, "\' failed for parameter setting ", row_idx, ". Aborting and returning ordered list of arguments. Error message was as follows:"))
        message(e)
        message("\n")
        return(list(OK = FALSE, ordered_args = ordered_args))
      },
      warning = function(w) {
        message(paste0("Method function \'", method_name, "\' produced a warning for parameter setting ", row_idx, ". Warning was as follows:"))
        message(w)
        message("\n")
        return(list(OK = TRUE, result_df = result_df, time = time))
      })
      if (out$OK) {
        result_lists[[method_name]][[row_idx]] <- out$result_df
        method_times[[method_name]][row_idx] <- out$time
      } else {
        return(out$ordered_args)
      }
    }
  }

  return(list(data = data_lists, results = result_lists, data_generation_times = data_generation_times, method_times = method_times))
}


#' Assign variables to global environment
#'
#' This function is meant to be used in conjunction with `check_simulatr_specifier_object`. The latter function detects
#' errors in a `simulatr_specifier` object. When an error occurs, `check_simulatr_specifier_object` returns the list of arguments
#' passed to a function in `simulatr_specifier`, along with an informative error message. The function `assign_vars_to_global_env`
#' takes a list of arguments, a `simulatr_specifier` object, and the name of a function in the `simulatr_specifier` object, and assigns
#' the named variables with values initialized from the list of arguments to the global environment.
#'
#' @param simulatr_spec a `simulatr_specifier` object
#' @param function_name the name of a function in the object; either `generate_data_function` for the data generation function, or the name of a specific method function.
#' @param var_values the ordered list of arguments passed to the function.
#'
#' @return NULL; has the side effect of assigning the values in the list to the named arguments in the global environment.
#' @export
assign_vars_to_global_env <- function(simulatr_spec, function_name, var_values) {
  if (function_name == "generate_data_function") {
    arg_names <- simulatr_spec@generate_data_function@arg_names
  } else {
    arg_names <- simulatr_spec@run_method_functions[[function_name]]@arg_names
    var_values <- var_values[-1]
  }
  # perform the assignment to the global environment
  for (i in seq(1, length(arg_names))) {
    assign(x = arg_names[i], value = var_values[[i]], pos = globalenv())
  }
  return(NULL)
}
