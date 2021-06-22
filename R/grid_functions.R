#' Create param grid
#'
#' Creates a parameter grid. For each variable, varies the value of that variable while holding the other variables constant at a specified value.
#'
#' @param varying_values a named list giving the values over which to vary the variables
#' @param baseline_values the baseline values of the variables; the variables take this value when not varying
#'
#' @return a parameter grid
#' @export
#'
#' @examples
#' varying_values <- list(x = seq(-2,2,0.1),
#' y = seq(0,2,0.1),
#' z = c("cond1", "cond2"))
#' baseline_values <- list(x = 0, y = 0.5, z = "cond1")
#' param_df <- create_param_grid(varying_values, baseline_values)
create_param_grid <- function(varying_values, baseline_values) {
  var_names <- names(varying_values)
  df_pieces <- lapply(var_names, function(curr_var) {
    varying <- varying_values[curr_var]
    fixed <- baseline_values[names(baseline_values) != curr_var]
    l_varying <- length(varying[[1]])
    curr_val_fixed_loc <- varying_values[[curr_var]] == baseline_values[[curr_var]]
    arm_bool <- lapply(var_names, function(i) {
      if (i == curr_var) {
        rep(TRUE, times = l_varying)
      } else {
        if (baseline_values[[i]] %in% varying_values[[i]]) curr_val_fixed_loc else rep(FALSE, l_varying)
      }
    }) %>% purrr::set_names(paste0("arm_", var_names))
    data.frame(varying, fixed, arm_bool)
  })
  grid_df <- do.call(rbind, df_pieces)
  idx_to_keep <- !duplicated(dplyr::select(grid_df, var_names))
  grid_df[idx_to_keep,]
}
