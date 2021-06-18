#' Check simulatr specifier object
#'
#' Checks a simulatr specifier object for correctness.
#'
#' @param simulatr_spec a simulatr_specifier object
#' @param B_in (default determined by simulatr_spec) number of resamples to use
#' @param parallel (default true) parallelize execution?
#'
#' @return if no errors, the list of results; if errors occur, the ordered list of arguments passed to the function in which the error occurred.
#' @export
#'
#' @examples
#' simulatr_spec <- readRDS("/Users/timbarry/research_offsite/glmeiv/private/simulations/sim_spec_1.rds")
#' simulatr_spec@parameter_grid  <- simulatr_spec@parameter_grid[1:10,]
#' simulatr_spec@fixed_parameters[["n"]] <- 2000
#' check <- check_simulatr_specifier_object(simulatr_spec, 5)
check_simulatr_specifier_object <- function(simulatr_spec, B_in = NULL, parallel = TRUE) {
  # decide which lapply function to use
  if (parallel) {
    future::plan(future::multisession())
    my_lapply <- function(X, FUN) future.apply::future_lapply(X, FUN, future.seed = NULL)
  } else {
    my_lapply <- lapply
  }
  # check: system.time(my_lapply(seq(1,3), function(i) Sys.sleep(1)))

  # set basic quantities
  n_param_settings <- nrow(simulatr_spec@parameter_grid)

  # update B in the fixed parameter list
  if (!is.null(B_in)) {
    simulatr_spec@fixed_parameters[["B"]] <- B_in
    if ("B" %in% colnames(simulatr_spec@parameter_grid)) simulatr_spec@parameter_grid$B <- NULL
  }

  # set up for data generation across parameter settings; load data generation packages (if necessary)
  data_generator <- simulatr_spec@generate_data_function
  packs_to_load <- data_generator@packages
  if (!(identical(packs_to_load, NA_character_))) invisible(lapply(packs_to_load, function(pack) library(pack, character.only = TRUE)))

  cat("Generating data.\n")
  # Generate the synthetic data
  data_generation_out <- my_lapply(X = seq(1, n_param_settings), FUN = function(row_idx) {
    # set seed for given row
    seed <- get_param_from_simulatr_spec(simulatr_spec, row_idx, "seed")
    set.seed(seed)
    # obtain arguments
    ordered_args <- lapply(data_generator@arg_names, function(curr_arg) {
      get_param_from_simulatr_spec(simulatr_spec, row_idx, curr_arg)
    })
    # obtain B; if B_in has been passed, use that instead
    B <- get_param_from_simulatr_spec(simulatr_spec, row_idx, "B")
    # generate the data, while clocking the time and looking for errors
    tryCatch({
      time <- system.time(
        if (data_generator@loop) {
          data_list <- replicate(B, do.call(data_generator@f, ordered_args), FALSE)
        } else {
          data_list <- do.call(data_generator@f, ordered_args)
        })[["elapsed"]]/B
      return(list(error = FALSE, warning = FALSE, time = time, data_list = data_list))
      # handle errors and warnings
    }, error = function(e) {
      return(list(error = TRUE, warning = FALSE, ordered_args = ordered_args, msg = e))
    }, warning = function(w) {
      return(list(error = FALSE, warning = TRUE, time = NA, data_list = data_list, msg = w))
    })
  })
  query_funct <- check_funct_helper(data_generation_out, "data generator")
  if (query_funct$stop_funct) return(query_funct$ret_val)
  # no errors; get the times and data_lists
  data_generation_times <- sapply(data_generation_out, function(i) i$time)
  data_lists <- lapply(data_generation_out, function(i) i$data_list)

  # Next, apply each method to each simulated dataset
  method_names <- names(simulatr_spec@run_method_functions)
  n_methods <- length(method_names)
  result_lists <- method_times <- vector(mode = "list", length = n_methods)
  names(result_lists) <- names(method_times) <- method_names
  for (method_name in method_names) {
    cat(paste0("Running method \'", method_name, "\'.\n"))
    method_object <- simulatr_spec@run_method_functions[[method_name]]
    packs_to_load <- method_object@packages
    if (!(identical(packs_to_load, NA_character_))) invisible(lapply(packs_to_load, function(pack)
      library(pack, character.only = TRUE)))
    # run the method across all parameter settings
    method_out <- my_lapply(seq(1, n_param_settings), function(row_idx) {
      tryCatch({
        # set the seed
        seed <- get_param_from_simulatr_spec(simulatr_spec, row_idx, "seed")
        set.seed(seed)
        # obtain arguments
        ordered_args <- c(list(NA), lapply(method_object@arg_names, function(curr_arg) {
          get_param_from_simulatr_spec(simulatr_spec, row_idx, curr_arg)}))
        # get the current data list
        data_list <- data_lists[[row_idx]]
        # get B
        B <- length(data_list)
        # run method, while clocking time and looking for errors
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
        return(list(error = FALSE, warning = FALSE, time = time, result_df = result_df))
      }, error = function(e) {
        return(list(error = TRUE, warning = FALSE, ordered_args = ordered_args, msg = e))
      }, warning = function(w) {
        return(list(error = FALSE, warning = TRUE, time = NA, result_df = result_df, msg = w))
      })
    })
    query_funct <- check_funct_helper(method_out, method_name)
    if (query_funct$stop_funct) return(query_funct$ret_val)
    # no errors; get the times and result_dfs
    method_times[[method_name]] <- sapply(method_out, function(i) i$time)
    result_lists[[method_name]] <- lapply(method_out, function(i) i$result_df)
  }
  cat("The input simulatr specifier object is specified correctly.\n")
  return(list(data = data_lists, results = result_lists, data_generation_times = data_generation_times, method_times = method_times))
}


check_funct_helper <- function(out_list, funct_name) {
  errors <- sapply(out_list, function(i) i$error)
  warnings <- sapply(out_list, function(i) i$warning)
  # helper printing function
  f <- function(issues, funct_name, issue_type) {
    issue_idxs <- which(issues)
    msg1 <- paste0("The \'", funct_name, "\' function produced ", issue_type,"s for the following parameter grid rows: ")
    msg2 <- paste0(issue_idxs, collapse = ",")
    msg3 <- paste0(msg1, msg2, ". The ", issue_type," messages were as follows: \n")
    cat(msg3)
    for (issue_idx in issue_idxs) {
      cat(paste0("Grid row ", issue_idx, ": "))
      message(out_list[[issue_idx]]$msg); cat("\n")
    }
  }
  # define default output
  ret <- list(stop_funct = FALSE)
  # check errors
  if (any(errors)) {
    f(errors, funct_name, "error")
    cat("Aborting function and returning list of arguments corresponding to each row that produced an error.\n")
    ret_val <- lapply(out_list[errors], function(i) i$ordered_args)
    ret <- list(stop_funct = TRUE, ret_val = ret_val)
  }
  # check warnings
  if (any(warnings)) {
    f(warnings, funct_name, "warning")
  }
  return(ret)
}

