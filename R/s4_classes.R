simulatr_function <- setClass("simulatr_function",
                              slots = list(f = "function",
                                           arg_names = "character",
                                           packages = "character",
                                           loop = "logical"))

simulatr_specifier <- setClass("simulatr_specifier",
                                   slots = list(parameter_grid = "data.frame",
                                                fixed_parameters = "list",
                                                generate_data_function = "simulatr_function",
                                                run_method_functions = "list"))

#' simulatr_function
#'
#' A `simulator_function` is a class that stores a method or data generation function alongside associated metadata.
#'
#' @param f a function with one or more arguments.
#' @param arg_names the (ordered) names of parameters (from the parameter grid or list of fixed parameters) to pass to f.
#' @param packages packages to load before calling f, stored as a character vector.
#' @param loop boolean indicating whether f should be called within a loop.
#'
#' @return A `simulatr_function` object
#' @export
#'
#' @examples
#' # an example data generation function
#' generate_lm_data <- function(beta_0, beta_1, sigma, n) {
#' x <- rnorm(n)
#' ep <- rnorm(n, sd = sigma)
#' y <- beta_0 + beta_1 * x + ep
#' data.frame(y = y, x = x)
#' }
#' data_generator <- simulatr_function(
#' f = generate_lm_data,
#' arg_names = c("beta_0", "beta_1", "sigma", "n"),
#' loop = TRUE)
#'
#' # an example method function
#' fit_lm <- function(df) {
#' fit <- lm(y ~ x, data = df)
#' s <- summary(fit)$coefficients
#' row.names(s) <- NULL
#' out <- data.frame(parameter = c("beta_0", "beta_1"),
#' Estimate = s[,"Estimate"],
#' p_val = s[,"Pr(>|t|)"]) %>% pivot_longer(
#' cols = c("Estimate", "p_val"),
#' names_to = "target")
#' return(out)
#' }
#' method <- simulatr_function(f = fit_lm, packages = "tidyr")
#'
#' # generate data and run method
#' df <- generate_lm_data(-2, 2, 1, 100)
#' lapply(method@packages, require, character.only = TRUE)
#' fit_lm(df)
simulatr_function <- function(f, arg_names = NA_character_, packages = NA_character_, loop = FALSE) {
  new(Class = "simulatr_function",
      f = f,
      arg_names = arg_names,
      packages = packages,
      loop = loop)
}


#' simulatr_specifier
#'
#' A `simulatr_specifier` object defines a simulation study in the `simulatr` framework.
#'
#' @param parameter_grid a named data frame storing the values of study parameters that vary
#' @param fixed_parameters a named list storing the values of the parameters that stay fixed across parameter settings
#' @param generate_data_function a `simulatr_function` that generates the data
#' @param run_method_functions a named list of `simulatr_functions` that define the methods to be run in the simulation
#'
#' @return a `simulatr_specifier` object
#' @export
#'
#' @examples
#' # an example data generation function
#' generate_lm_data <- function(beta_0, beta_1, sigma, n) {
#' x <- rnorm(n)
#' ep <- rnorm(n, sd = sigma)
#' y <- beta_0 + beta_1 * x + ep
#' data.frame(y = y, x = x)
#' }
#' data_generator <- simulatr_function(
#' f = generate_lm_data,
#' arg_names = c("beta_0", "beta_1", "sigma", "n"),
#' loop = TRUE)
#'
#' # an example method function
#' fit_lm <- function(df) {
#' fit <- lm(y ~ x, data = df)
#' s <- summary(fit)$coefficients
#' row.names(s) <- NULL
#' out <- data.frame(parameter = c("beta_0", "beta_1"),
#' Estimate = s[,"Estimate"],
#' p_val = s[,"Pr(>|t|)"]) %>% pivot_longer(
#' cols = c("Estimate", "p_val"),
#' names_to = "target")
#' return(out)
#' }
#' method <- simulatr_function(f = fit_lm, packages = "tidyr")
#'
#' # defining the additional parameters to pass to simulatr_specifier
#' parameter_grid <- expand.grid(beta_0 = seq(-2, 2, 1), beta_1 = seq(-2, 2, 1))
#' fixed_parameters <- list(n = 1000, sigma = 1)
#'
#' # finally, obtain simulatr_specifier object
#' simulation <- simulatr_specifier(
#' parameter_grid = parameter_grid,
#' fixed_parameters = fixed_parameters,
#' generate_data_function = data_generator,
#' run_method_functions = list(lm = generate_lm_data))
simulatr_specifier <- function(parameter_grid, fixed_parameters, generate_data_function, run_method_functions) {
  new(Class = "simulatr_specifier",
      parameter_grid = parameter_grid,
      fixed_parameters = fixed_parameters,
      generate_data_function = generate_data_function,
      run_method_functions = run_method_functions)
}


#' get example simulatr_specifier
#'
#' @return an example simulatr specifier object
#' @examples
#' load_all()
#' get_example_simulatr_specifier()
get_example_simulatr_specifier <- function() {
  generate_lm_data <- function(beta_0, beta_1, sigma, n) {
    x <- rnorm(n)
    ep <- rnorm(n, sd = sigma)
    y <- beta_0 + beta_1 * x + ep
    data.frame(y = y, x = x)
  }
  data_generator <- simulatr_function(
    f = generate_lm_data,
    arg_names = c("beta_0", "beta_1", "sigma", "n"),
    loop = TRUE)

  fit_lm <- function(df) {
    fit <- lm(y ~ x, data = df)
    s <- summary(fit)$coefficients
    row.names(s) <- NULL
    out <- data.frame(
      parameter = c("beta_0", "beta_1"),
      Estimate = s[, "Estimate"],
      p_val = s[, "Pr(>|t|)"] ) %>% pivot_longer(
        cols = c("Estimate", "p_val"),
        names_to = "target")
    return(out)
  }
  method <- simulatr_function(f = fit_lm, packages = "tidyr", loop = TRUE)

  silly_competitor <- function(df, sigma) {
    out <- data.frame(parameter = c("beta_1", "beta_1"),
                      target = c("Estimate", "p_val"),
                      value = c(rnorm(1, sd = sigma), runif(1)))
    return(out)
  }
  competitor <- simulatr_function(f = silly_competitor, arg_names = "sigma", loop = TRUE)

  parameter_grid <- expand.grid(beta_0 = seq(-2, 2, 1), beta_1 = seq(-2, 2, 1))
  fixed_parameters <- list(n = 1000, sigma = 1, n_cores = 2)

  simulatr_specifier_object <- simulatr_specifier(
    parameter_grid = parameter_grid,
    fixed_parameters = fixed_parameters,
    generate_data_function = data_generator,
    run_method_functions = list(lm = method, silly = competitor)
  )
  return(simulatr_specifier_object)
}
