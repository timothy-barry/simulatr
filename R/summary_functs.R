#' Summarize results
#'
#' Summarizes the results of a `simulatr` simulation.
#'
#' @param sim_spec a `simulatr_specifier` object
#' @param sim_res the data frame of raw results, as outputted by `simulatr`
#' @param metrics character vector of metrics to compute; options include "coverage," "bias," "se" (standard deviation of estimator), "mse", "rejection_probability," "count," and "time."
#' @param parameters character vector of parameters on which to compute the metrics
#' @param threshold (optional; default value 0.05) the rejection threshold to use for "rejection_probability" metric.
#' @return a data frame of summarized results
#' @export
summarize_results <- function(sim_spec, sim_res, metrics, parameters, threshold = 0.05) {
  sim_res$grid_row_id <- as.integer(as.character(sim_res$grid_row_id))
  # get the functions to apply
  funts_to_apply <- purrr::set_names(paste0("compute_", metrics), metrics)
  funts_to_apply <- funts_to_apply[funts_to_apply != "compute_time"]

  # the named list of arguments to pass to each function
  arg_names_list <- list(compute_bias = c("tbl", "key", "sim_spec"),
                         compute_coverage = c("tbl", "key", "sim_spec"),
                         compute_rejection_probability = c("tbl", "threshold"),
                         compute_count = "tbl",
                         compute_mse = c("tbl", "key", "sim_spec"),
                         compute_se = "tbl",
                         compute_ci_width = "tbl")
  # initialize bag_of_vars
  bag_of_vars <- new.env()
  bag_of_vars$threshold <- threshold; bag_of_vars$sim_spec <- sim_spec
  # apply functions to each group in results tibble
  summary_stats <- sim_res %>%
    dplyr::filter(parameter %in% parameters) %>%
    dplyr::group_by(grid_row_id, parameter, method) %>%
    dplyr::group_modify(.f = function(tbl, key) {
      # put tbl and key into bag_of_vars
      bag_of_vars$tbl <- tbl; bag_of_vars$key <- key
      # map over the functions to apply; extract relevant args from bag, and call function
      purrr::map_dfr(.x = funts_to_apply,
                     .f = function(f_name) {
                       curr_args <- mget(arg_names_list[[f_name]], bag_of_vars)
                       do.call(what = f_name, args = curr_args)
                     },
                     .id = "metric")
    }) %>% dplyr::ungroup() %>% dplyr::mutate(grid_row_id = as.integer(grid_row_id))

  # compute time, if requested (not associated with any specific parameter)
  if ("time" %in% metrics) {
    time_df <- sim_res %>% dplyr::group_by(grid_row_id, method) %>%
      dplyr::group_modify(.f = function(tbl, key) {
        times <- dplyr::filter(tbl, target == "time") %>% dplyr::pull(value)
        se <- stats::sd(times)
        m <- mean(times)
        n_sim <- length(times)
        lower_mc_ci <- m - 1.96 * se / sqrt(n_sim)
        upper_mc_ci <- m + 1.96 * se / sqrt(n_sim)
        dplyr::tibble(value =  m, lower_mc_ci = lower_mc_ci, upper_mc_ci = upper_mc_ci)
      }) %>% dplyr::mutate(parameter = "meta", metric = "time")
    summary_stats <- rbind(time_df, summary_stats)
  }

  # combine with param grid
  param_grid <- sim_spec@parameter_grid
  if (!("grid_row_id") %in% colnames(param_grid)) {
    param_grid <- tibble::tibble(grid_row_id = seq(1, nrow(param_grid))) %>% dplyr::mutate(param_grid)
  }
  out <- dplyr::inner_join(x = param_grid, y = summary_stats, by = "grid_row_id")

  # add 0 counts back in if necessary
  if ("count" %in% metrics) {
    null_count_df <- expand.grid(parameter = parameters,
                                 method = names(sim_spec@run_method_functions),
                                 grid_row_id = seq(1, nrow(param_grid)),
                                 metric = "count",
                                 null_value = 0) %>%
      dplyr::left_join(x = ., y = param_grid, by = "grid_row_id")
    out <- dplyr::full_join(null_count_df, out) %>%
      dplyr::mutate(value = ifelse(is.na(value), null_value, value)) %>%
      dplyr::select(-null_value)
  }
  out <- tibble::as_tibble(x = out)
  return(out)
}


#' Compute bias
#'
#' Computes the bias of an estimator.
#'
#' @param tbl data frame with columns target and value. Target should have entry "estimate."
#' @param key data frame with columns for parameter and grid_row_id
#' @param sim_spec a simulatr specifier object
#'
#' @return a 1-row tibble with columns value, lower_mc_ci, and upper_mc_ci
compute_bias <- function(tbl, key, sim_spec) {
  parameter <- as.character(key$parameter); grid_row_id <- key$grid_row_id
  ground_truth <- get_param_from_simulatr_spec(sim_spec, grid_row_id, parameter)
  ests <- dplyr::filter(tbl, target == "estimate") %>% dplyr::pull(value)
  ests <- ests[!is.na(ests)]
  n_sim <- length(ests)
  sample_mean <- mean(ests)
  sample_bias <- sample_mean - ground_truth
  mc_se <- sqrt( 1/(n_sim * (n_sim - 1)) * sum((ests - sample_mean)^2) )
  dplyr::tibble(value = sample_bias, lower_mc_ci = sample_bias - 1.96 * mc_se, upper_mc_ci = sample_bias + 1.96 * mc_se)
}


#' Compute mean squared error
#'
#' Computes the MSE of an estimator.
#'
#' @param tbl similar to bias
#' @param key similar to bias
#' @param sim_spec similar to bias
#'
#' @return similar to bias
compute_mse <- function(tbl, key, sim_spec) {
  parameter <- as.character(key$parameter); grid_row_id <- key$grid_row_id
  ground_truth <- get_param_from_simulatr_spec(sim_spec, grid_row_id, parameter)
  ests <- dplyr::filter(tbl, target == "estimate") %>% dplyr::pull(value)
  ests <- ests[!is.na(ests)]
  n_sim <- length(ests)
  mse <- (1/n_sim) * sum((ests - ground_truth)^2)
  mc_se <- sqrt(sum(((ests - ground_truth)^2 - mse)^2)/(n_sim * (n_sim - 1)))
  dplyr::tibble(value = mse, lower_mc_ci = mse - 1.96 * mc_se, upper_mc_ci = mse + 1.96 * mc_se)
}


#' Compute se
#'
#' @param tbl similar to bias
#'
#' @return similar to bias
compute_se <- function(tbl) {
  ests <- dplyr::filter(tbl, target == "estimate") %>% dplyr::pull(value)
  ests <- ests[!is.na(ests)]
  n_sim <- length(ests)
  se <- stats::sd(ests)
  mc_se <- se/sqrt(2 * (n_sim - 1))
  dplyr::tibble(value = se, lower_mc_ci = se - 1.96 * mc_se, upper_mc_ci = se + 1.96 * mc_se)
}

#' Compute coverage
#'
#' @param tbl data frame with columns target, value, and id. Column "target" should have entries "confint_lower" and "confint_higher"
#' @param key data frame with columns for parameter and grid_row_id
#' @param sim_spec a simulatr specifier object
#'
#' @return a 1-row tibble with columns value, lower_mc_ci, and upper_mc_ci
compute_coverage <- function(tbl, key, sim_spec) {
  parameter <- as.character(key$parameter); grid_row_id <- key$grid_row_id
  ground_truth <- get_param_from_simulatr_spec(sim_spec, grid_row_id, parameter)
  covered <- dplyr::filter(tbl, target %in% c("confint_lower", "confint_upper")) %>%
    dplyr::group_by(id) %>%
    dplyr::summarize(covered = (value[target == "confint_lower"] < ground_truth
                                & value[target == "confint_upper"] > ground_truth)) %>%
    dplyr::pull(covered)
  covered <- covered[!is.na(covered)]
  n_sim <- length(covered)
  coverage <- mean(covered)
  mc_se <- sqrt((coverage * (1 - coverage))/n_sim)
  dplyr::tibble(value = coverage, lower_mc_ci = coverage - 1.96 * mc_se, upper_mc_ci = coverage + 1.96 * mc_se)
}


compute_ci_width <- function(tbl) {
  widths <- dplyr::filter(tbl, target %in% c("confint_lower", "confint_upper")) %>%
    dplyr::select(target, value, id) %>%
    tidyr::pivot_wider(id_cols = c("id"), names_from = "target", values_from = "value") %>%
    dplyr::summarize(confint_upper - confint_lower) %>% dplyr::pull()
  dplyr::tibble(value = mean(widths), lower_mc_ci = NA, upper_mc_ci = NA)
}


#' Computes rejection probability
#'
#' Rejection probability is type-I error (under null) and type-II error (under alternative)
#'
#' @param tbl data frame with columns target, value, and id. Column "target" should have entry "p_value."
#' @param threshold rejection threshold
#'
#' @return a 1-row tibble with columns value, lower_mc_ci, and upper_mc_ci
compute_rejection_probability <- function(tbl, threshold) {
  p_vals <- dplyr::filter(tbl, target == "p_value") %>% dplyr::pull(value)
  p_vals <- p_vals[!is.na(p_vals)]
  n_sim <- length(p_vals)
  rejection_probability <- mean(p_vals < 0.05)
  mc_se <- sqrt((rejection_probability * (1 - rejection_probability))/n_sim)
  dplyr::tibble(value = rejection_probability, lower_mc_ci = rejection_probability - 1.96 * mc_se, upper_mc_ci = rejection_probability + 1.96 * mc_se)
}


compute_count <- function(tbl) {
  count <- tbl$id %>% as.character() %>% unique() %>% length()
  dplyr::tibble(value = count, lower_mc_ci = NA, upper_mc_ci = NA)
}


#' Evaluate the FDP of a discovery set
#'
#' @param output The output of a method, an object that must contain the field nonnulls
#' @param ground_truth The ground truth, an object that must contain the field nonnulls
#'
#' @return The FDP
#' @export
fdp = function(output, ground_truth){
  length(setdiff(output$nonnulls, ground_truth$nonnulls))/max(1, length(output$nonnulls))
}

#' Evaluate the power of a discovery set
#'
#' @param output The output of a method, an object that must contain the field nonnulls
#' @param ground_truth The ground truth, an object that must contain the field nonnulls
#'
#' @return The power
#' @export
power = function(output, ground_truth){
  length(intersect(output$nonnulls, ground_truth$nonnulls))/max(1, length(ground_truth$nonnulls))
}