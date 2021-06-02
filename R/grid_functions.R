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
    data.frame(varying, fixed)
  })
  do.call(rbind, df_pieces)
}
