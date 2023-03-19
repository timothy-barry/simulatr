#' simulatr_function
#'
#' A `simulator_function` is a class that stores a method or data generation function alongside associated metadata.
#'
#' @param f a function with one or more arguments.
#' @param arg_names the (ordered) names of parameters (from the parameter grid or list of fixed parameters) to pass to f.
#' @param packages packages to load before calling f, stored as a character vector.
#' @param loop boolean indicating whether f should be called within a loop.
#' @param one_rep_time amount of time it takes for function to exeecute setting B = 1 (optional)
#' @param mult_time_factor time requested for given function is (one_rep_time)(B)(mult_time_factor)/n_processors + add_time_factor
#' @param add_time_factor see above
#'
#' @return A `simulatr_function` object
#' @export
simulatr_function <- function(f,
                              arg_names = NA_character_,
                              packages = NA_character_,
                              loop = FALSE,
                              one_rep_time = NA_real_,
                              mult_time_factor = 2,
                              add_time_factor = 60
                              ) {
  new(Class = "simulatr_function",
      f = f,
      arg_names = arg_names,
      packages = packages,
      loop = loop,
      one_rep_time = one_rep_time,
      mult_time_factor = mult_time_factor,
      add_time_factor = add_time_factor)
}


#' simulatr_specifier
#'
#' A `simulatr_specifier` object defines a simulation study in the `simulatr` framework.
#'
#' @param parameter_grid a named data frame storing the values of study parameters that vary
#' @param fixed_parameters a named list storing the values of the parameters that stay fixed across parameter settings
#' @param generate_data_function a `simulatr_function` that generates the data
#' @param run_method_functions a named list of `simulatr_functions` that define the methods to be run in the simulation
#' @param evaluation_functions a named list of functions that define the metrics used to evaluate each method
#'
#' @return a `simulatr_specifier` object
#' @export
simulatr_specifier <- function(parameter_grid,
                               fixed_parameters,
                               generate_data_function,
                               run_method_functions,
                               evaluation_functions = list()) {
  new(Class = "simulatr_specifier",
      parameter_grid = parameter_grid,
      fixed_parameters = fixed_parameters,
      generate_data_function = generate_data_function,
      run_method_functions = run_method_functions,
      evaluation_functions = evaluation_functions)
}


#' get example simulatr_specifier
#'
#' @return an example simulatr specifier object
get_example_simulatr_specifier <- function() {
  ##########################
  # Data generation function
  ##########################
  # Function
  generate_lm_data <- function(beta_0, beta_1, sigma, n) {
    x <- stats::rnorm(n)
    ep <- stats::rnorm(n, sd = sigma)
    y <- beta_0 + beta_1 * x + ep
    data.frame(y = y, x = x)
  }

  # Simulatr function object
  data_generator <- simulatr_function(f = generate_lm_data,
                                      arg_names = c("beta_0", "beta_1", "sigma", "n"),
                                      loop = TRUE)

  ##################
  # Method 1: fit lm
  ##################
  # method
  fit_lm <- function(df) {
    fit <- stats::lm(y ~ x, data = df)
    s <- summary(fit)$coefficients
    row.names(s) <- NULL
    out <- data.frame(
      parameter = c("beta_0", "beta_1"),
      Estimate = s[, "Estimate"],
      p_val = s[, "Pr(>|t|)"]) %>% tidyr::pivot_longer(
        cols = c("Estimate", "p_val"),
        names_to = "target")
    return(out)
  }
  # Simulatr function object
  method <- simulatr_function(f = fit_lm, packages = "tidyr", loop = TRUE)

  ############################
  # Method 2: silly competitor
  ############################
  # method
  silly_competitor <- function(df, sigma) {
    out <- data.frame(parameter = c("beta_1", "beta_1"),
                      target = c("Estimate", "p_val"),
                      value = c(stats::rnorm(1, sd = sigma), stats::runif(1)))
    return(out)
  }
  # Simulatr function object
  competitor <- simulatr_function(f = silly_competitor, arg_names = "sigma", loop = TRUE)

  #####################################
  # Paramater grid and fixed parameters
  #####################################
  parameter_grid <- expand.grid(beta_0 = seq(-2, 2, 2), beta_1 = seq(-2, 2, 2))
  fixed_parameters <- list(n = 100, sigma = 1, n_processors = 2, seed = 4, B = 100)

  ###########################
  # Simulatr specifier object
  ###########################
  simulatr_specifier_object <- simulatr_specifier(
    parameter_grid = parameter_grid,
    fixed_parameters = fixed_parameters,
    generate_data_function = data_generator,
    run_method_functions = list(lm = method, silly = competitor)
  )
  return(simulatr_specifier_object)
}
