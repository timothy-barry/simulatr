#' Summarize results
#'
#' Summarizes the results of a `simulatr` simulation.
#'
#' @param sim_spec a `simulatr_specifier` object
#' @param sim_res the data frame of raw results, as outputted by `simulatr`
#' @param metrics character vector of metrics to compute; options include "coverage," "bias," "rejection_probability," and "count."
#' @param parameters character vector of parameters on which to compute the metrics
#' @param threshold (optional; default value 0.05) the rejection threshold to use for "rejection_probability" metric.
#' @return a data frame of summarized results
#' @export
summarize_results <- function(sim_spec, sim_res, metrics, parameters, threshold = 0.05) {
  sim_res$grid_row_id <- as.integer(as.character(sim_res$grid_row_id))
  # get the functions to apply
  funts_to_apply <- purrr::set_names(paste0("compute_", metrics), metrics)
  # the named list of arguments to pass to each function
  arg_names_list <- list(compute_bias = c("tbl", "key", "sim_spec"),
                     compute_coverage = c("tbl", "key", "sim_spec"),
                     compute_rejection_probability = c("tbl", "threshold"),
                     compute_count = "tbl")
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

  # add count of zero back in
  if ("count" %in% metrics) {
    methods <- names(sim_spec@run_method_functions)
    missing <- summary_stats %>% filter(metric == "count") %>% group_by(grid_row_id, parameter) %>%
      summarize(method = methods[!(methods %in% method)]) %>%
      mutate(metric = "count", value = 0, lower_mc_ci = NA, upper_mc_ci = NA)
    summary_stats <- rbind(summary_stats, missing)
  }
  # combine with param grid
  param_grid <- sim_spec@parameter_grid
  if (!("grid_row_id") %in% colnames(param_grid)) {
    param_grid <- tibble::tibble(grid_row_id = seq(1, nrow(param_grid))) %>% dplyr::mutate(param_grid)
  }
  out <- dplyr::inner_join(x = param_grid, y = summary_stats, by = "grid_row_id")
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
  covered <- dplyr::filter(tbl, target %in% c("confint_lower", "confint_higher")) %>%
    dplyr::group_by(id) %>%
    dplyr::summarize(covered = (value[target == "confint_lower"] < ground_truth
                                & value[target == "confint_higher"] > ground_truth)) %>%
    dplyr::pull(covered)
  covered <- covered[!is.na(covered)]
  n_sim <- length(covered)
  coverage <- mean(covered)
  mc_se <- sqrt((coverage * (1 - coverage))/n_sim)
  dplyr::tibble(value = coverage, lower_mc_ci = coverage - 1.96 * mc_se, upper_mc_ci = coverage + 1.96 * mc_se)
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


#' Plot all arms
#'
#' Plots all arms of a simulation study for a given parameter and metric.
#'
#' @param summarized_results a data frame outputted by `summarize_results.`
#' @param parameter name of the target parameter
#' @param metric name of the metric used to quantify inference on the target parameter
#'
#' @return a plot
#' @export
plot_all_arms <- function(summarized_results, parameter, metric, ylim = NULL) {
  arms <- grep(pattern = "^arm_", x = colnames(summarized_results), value = TRUE) %>% gsub(pattern = "^arm_", replacement = "", x = .)
  summarized_results_sub <- dplyr::filter(summarized_results, parameter == !!parameter)
  ps <- lapply(arms, function(arm) {
    arm_name <- paste0("arm_", arm)
    other_arms <- arms[ !(arms == arm) ]
    title <- sapply(other_arms, function(other_arm) paste0(other_arm, " = ", summarized_results_sub[[other_arm]][1]))
    title <- paste0(paste0(title, collapse = ", "))
    to_plot <- dplyr::filter(summarized_results_sub, !!as.symbol(arm_name) & metric == !!metric)
    p <- ggplot2::ggplot(to_plot, ggplot2::aes(x = !!as.symbol(arm), y = value, col = method)) + ggplot2::geom_point() + ggplot2::geom_line() + ggplot2::ylab(metric) + ggplot2::geom_errorbar(ggplot2::aes(ymin = lower_mc_ci, ymax = upper_mc_ci), width = 0) + ggplot2::theme_bw() + ggplot2::theme(plot.title = ggplot2::element_text(size = 11, hjust = 0.5)) + ggplot2::ggtitle(title) + if (is.null(ylim)) NULL else ggplot2::ylim(ylim)
    l <- cowplot::get_legend(p + ggplot2::theme(legend.position = "bottom"))
    p_out <- p + ggplot2::theme(legend.position = "none")
    return(list(plot = p_out, legend = l))
  })
  n_ps <- length(ps)
  vert_plot <- cowplot::plot_grid(plotlist = lapply(ps, function(i) i$plot),
                         align = "v", axis = "l", nrow = n_ps, labels = letters[1:n_ps])
  out <- cowplot::plot_grid(vert_plot, ps[[1]]$legend, ncol = 1, rel_heights = c(1, .1))
  return(out)
}
