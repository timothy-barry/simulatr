#' Summarize results
#'
#' Summarizes the results of a `simulatr` simulation.
#'
#' @param simulatr_specifier a `simulatr_specifier` object
#' @param raw_result_df the data frame of raw results, as outputted by `simulatr`
#' @param metrics character vector of metrics to compute; options include coverage and bias.
#'
#' @return a data frame of summarized results
#' @export
#'
#' @examples
#' \dontrun{
#' simulatr_specifier <-
#' readRDS("/Users/timbarry/research_offsite/glmeiv/private/simulations/sim_spec_1.rds")
#' raw_result_df <-
#' readRDS("/Users/timbarry/research_offsite/glmeiv/private/simulations/result.rds")
#' raw_result_df <- dplyr::filter(raw_result_df,
#' parameter %in% c("m_perturbation", "g_perturbation", "pi"))
#' metrics <- c("bias", "coverage")
#' }
summarize_results <- function(simulatr_specifier, raw_result_df, metrics) {
  raw_result_df$grid_row_id <- as.integer(as.character(raw_result_df$grid_row_id))
  funts_to_apply <- purrr::set_names(paste0("compute_", metrics), metrics)
  summary_stats <- dplyr::group_by(raw_result_df, grid_row_id, parameter, method) %>% dplyr::group_modify(.f = function(tbl, key) {
    purrr::map_dfr(.x = funts_to_apply,
                   .f = function(f) do.call(f, args = list(tbl, key, simulatr_specifier)),
                   .id = "metric")
  }) %>% dplyr::ungroup() %>% dplyr::mutate(grid_row_id = as.integer(grid_row_id))
  param_grid <- simulatr_specifier@parameter_grid
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
#' @param simulatr_specifier a simulatr specifier object
#'
#' @return a 1-row tibble with columns value, lower_mc_ci, and upper_mc_ci
compute_bias <- function(tbl, key, simulatr_specifier) {
  parameter <- as.character(key$parameter); grid_row_id <- key$grid_row_id
  ground_truth <- get_param_from_simulatr_spec(simulatr_specifier, grid_row_id, parameter)
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
#' @param simulatr_specifier a simulatr specifier object
#'
#' @return a 1-row tibble with columns value, lower_mc_ci, and upper_mc_ci
compute_coverage <- function(tbl, key, simulatr_specifier) {
  parameter <- as.character(key$parameter); grid_row_id <- key$grid_row_id
  ground_truth <- get_param_from_simulatr_spec(simulatr_specifier, grid_row_id, parameter)
  covered <- dplyr::filter(tbl, target %in% c("confint_lower", "confint_higher")) %>%
    tidyr::pivot_wider(id_cols = "id", names_from = target, values_from = value) %>% stats::na.omit() %>%
    dplyr::mutate(covered = (confint_lower <= ground_truth && confint_higher >= ground_truth)) %>%
    dplyr::pull(covered)
  coverage <- mean(covered)
  n_sim <- length(covered)
  mc_se <- sqrt((coverage * (1 - coverage))/n_sim)
  dplyr::tibble(value = coverage, lower_mc_ci = coverage - 1.96 * mc_se, upper_mc_ci = coverage + 1.96 * mc_se)
}


