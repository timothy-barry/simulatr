#' Check simulatr specifier object
#'
#' Checks a simulatr specifier object for correctness.
#'
#' @param simulatr_spec a simulatr_specifier object
#' @param B_in (default determined by simulatr_spec) number of resamples to use
#' @param return_data (default false) whether to return the data 
#' @param parallel (default true) parallelize execution?
#'
#' @return if no errors, the list of results; if errors occur, the ordered list of arguments passed to the function in which the error occurred.
#' @export
#'
#' @examples
#' \dontrun{
#' simulatr_spec <-
#' readRDS("/Users/timbarry/research_offsite/glmeiv/private/simulations/sim_spec_1.rds")
#' simulatr_spec@parameter_grid  <- simulatr_spec@parameter_grid[1:10,]
#' simulatr_spec@fixed_parameters[["n"]] <- 2000
#' check <- check_simulatr_specifier_object(simulatr_spec, 5)
#' }
check_simulatr_specifier_object <- function(simulatr_spec, B_in = NULL, return_data = FALSE, parallel = TRUE) {
  # decide which lapply function to use
  if (parallel) {
    future::plan(future::multisession())
    my_lapply <- function(X, FUN) future.apply::future_lapply(X, FUN, future.seed = NULL)
  } else {
    my_lapply <- lapply
  }

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

  cat("Generating data...\n")
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
      invisible(gc(reset = TRUE)) # garbage collect prior to generating data
      time <- suppressMessages(system.time(
        if (data_generator@loop) {
          data_list <- replicate(B, do.call(data_generator@f, ordered_args), FALSE)
        } else {
          data_list <- do.call(data_generator@f, ordered_args)
        })[["elapsed"]]/B)
      bytes <- get_memory_used()/B
      return(list(error = FALSE, warning = FALSE, time = time,
                  bytes = bytes, data_list = data_list))
      # handle errors and warnings
    }, error = function(e) {
      return(list(error = TRUE, warning = FALSE, ordered_args = ordered_args, msg = e))
    }, warning = function(w) {
      return(list(error = FALSE, warning = TRUE, ordered_args = ordered_args, msg = w))
    })
  })
  query_funct <- check_funct_helper(data_generation_out, "data generator")
  if (query_funct$stop_funct) return(query_funct$ret_val)
  # no errors; get the times and data_lists
  data_generation_times <- sapply(data_generation_out, function(i) i$time)
  data_generation_bytes <- sapply(data_generation_out, function(i) i$bytes)
  data_lists <- lapply(data_generation_out, function(i) i$data_list)

  # Next, apply each method to each simulated dataset
  method_names <- names(simulatr_spec@run_method_functions)
  n_methods <- length(method_names)
  result_lists <- method_times <- method_bytes <- vector(mode = "list", length = n_methods)
  names(result_lists) <- names(method_times) <- names(method_bytes) <- method_names
  for (method_name in method_names) {
    cat(paste0("Running method \'", method_name, "\'...\n"))
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
        if (identical(method_object@arg_names, NA_character_)) {
          ordered_args <- list(NA)
        } else {
          ordered_args <- c(list(NA), lapply(method_object@arg_names, function(curr_arg) {
            get_param_from_simulatr_spec(simulatr_spec, row_idx, curr_arg)}))
        }
        # get the current data list
        data_list <- data_lists[[row_idx]]
        # get B
        B <- length(data_list)
        # run method, while clocking time and looking for errors
        invisible(gc(reset = TRUE)) # garbage collect prior to generating data
        time <- suppressMessages(system.time(if (method_object@loop) {
          result_list <- vector(mode = "list", length = length(data_list))
          for (i in seq(1, length(data_list))) {
            ordered_args[[1]] <- data_list[[i]]
            out <- dplyr::tibble(output = list(do.call(method_object@f, ordered_args)),
                          run_id = i)
            result_list[[i]] <- out
          }
          result_df <- do.call(rbind, result_list)
        } else {
          ordered_args[[1]] <- data_list
          result_df <- do.call(method_object@f, ordered_args)
        })[["elapsed"]]/B)
        bytes <- get_memory_used()/B
        result_df$grid_id <- row_idx
        return(list(error = FALSE, warning = FALSE, time = time, bytes = bytes, result_df = result_df))
      }, error = function(e) {
        return(list(error = TRUE, warning = FALSE, ordered_args = ordered_args, msg = e))
      }, warning = function(w) {
        return(list(error = FALSE, warning = TRUE, ordered_args = ordered_args, msg = w))
      })
    })
    query_funct <- check_funct_helper(method_out, method_name)
    if (query_funct$stop_funct) return(query_funct$ret_val)
    # no errors; get the times and result_dfs
    method_times[[method_name]] <- sapply(method_out, function(i) i$time)
    method_bytes[[method_name]] <- sapply(method_out, function(i) i$bytes)
    result_lists[[method_name]] <- do.call(what = rbind, args = lapply(method_out, function(i) i$result_df)) %>%
      dplyr::mutate(method = method_name)
  }
  n_warnings <- c(data_generation_times, unlist(method_times)) %>% is.na() %>% sum()
  if (n_warnings == 0) {
    cat("\nSUMMARY: The simulatr specifier object is specified correctly!\n")
  } else {
    cat(paste0("\nSUMMARY: There are ", n_warnings, " warnings (see above). Otherwise, simulatr specifier object is specified correctly.\n"))
  }
  results <- do.call(what = rbind, args = result_lists)
  
  # join the results with the parameter grid
  results_joined <- results |> 
    dplyr::left_join(simulatr_spec@parameter_grid |> 
                dplyr::mutate(grid_id = row_number()) |> 
                dplyr::select(grid_id, ground_truth), 
              by = "grid_id")
  
  # evaluate the metrics
  if(length(simulatr_spec@evaluation_functions) > 0){
    metrics <- lapply(names(simulatr_spec@evaluation_functions), function(fun_name){
      results_joined |> 
        dplyr::rowwise() |>
        dplyr::mutate(metric = fun_name, value = evaluation_functions[[fun_name]](output, ground_truth)) |>
        dplyr::ungroup()
    }) |>
      dplyr::bind_rows() |>
      dplyr::group_by(grid_id, method, metric) |>
      dplyr::summarise(mean = mean(value), se = sd(value)/sqrt(dplyr::n()), .groups = "drop") |>
      dplyr::left_join(simulatr_spec@parameter_grid |> 
                         dplyr::mutate(grid_id = dplyr::row_number()) |> 
                         dplyr::select(-ground_truth), 
                       by = "grid_id") |>
      dplyr::select(-grid_id) |>
      dplyr::relocate(method, metric, mean, se)
  } else{
    metrics <- NULL
  }
  
  # return
  output <- list(
    results = results,
    metrics = metrics,
    data_generation_times = data_generation_times,
    data_generation_bytes = data_generation_bytes,
    method_times = method_times,
    method_bytes = method_bytes
  )
  if(return_data){
    output$data = data_lists
  }
  output
}

check_funct_helper <- function(out_list, funct_name) {
  errors <- sapply(out_list, function(i) i$error)
  warnings <- sapply(out_list, function(i) i$warning)
  # helper printing function
  f <- function(issues, funct_name, issue_type) {
    issue_idxs <- which(issues)
    msg1 <- paste0("The \'", funct_name, "\' function produced ", issue_type,"s for the following parameter grid rows: ")
    msg2 <- paste0(issue_idxs, collapse = ",")
    msg3 <- paste0(msg1, msg2, ". The ", issue_type," messages are as follows: \n")
    cat(msg3)
    for (issue_idx in issue_idxs) {
      cat(paste0("Grid row ", issue_idx, ": "))
      message(out_list[[issue_idx]]$msg); cat("\n")
    }
    cat("\n")
  }
  # define default output
  ret <- list(stop_funct = FALSE)
  # check errors OR warnings
  if (any(errors) | any(warnings)) {
    if (any(errors)) {
      f(errors, funct_name, "error")
      msg_type <- "error"
      ret_val <- lapply(out_list[errors], function(i) i$ordered_args)
    } else {
      f(warnings, funct_name, "warning")
      msg_type <- "warning"
      ret_val <- lapply(out_list[warnings], function(i) i$ordered_args)
    }
    cat(paste0("Aborting and returning list of arguments corresponding to rows that produced ", msg_type,"s for function \`", funct_name, "\`.\n"))
    if (length(ret_val) == 1) ret_val <- ret_val[[1]]
    ret <- list(stop_funct = TRUE, ret_val = ret_val)
  }
  return(ret)
}

#' Compute memory used since last call to gc(reset = TRUE)
#'
#' @return Number of bytes used
#' @export
get_memory_used <- function(){
  BYTES_PER_NCELL <- 56
  BYTES_PER_VCELL <- 8
  gc_output <- gc()
  gc_output["Ncells","max used"]*BYTES_PER_NCELL +
    gc_output["Vcells","max used"]*BYTES_PER_VCELL
}
