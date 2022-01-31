library(simulatr)

set.seed(10)
# 1. Create the parameter grid of varying parameters
p_grid <- expand.grid(sigma = seq(0.5, 2, 0.5), beta_0 = c(0, 5))

# 2. Create the list of fixed parameters
n <- 1000
fixed_params <- list(betas = c(1, 2, -1, 4),
                     design_matrix = matrix(c(runif(n, 0, 5), rbinom(n, 1, 0.5),
                                            rnorm(n), runif(n, -2, 2)), ncol = 4),
                     B = 2, seed = 4, n_processors = 5)

# 3. create the data generation function
generate_data_f <- function(sigma, beta_0, betas, design_matrix, B) {
  l <- beta_0 + as.numeric(design_matrix %*% betas)
  n <- length(l)
  out <- replicate(n = B, expr = {
    ep <- rnorm(n = n, mean = 0, sd = sigma)
    l + ep
  }, simplify = FALSE)
  return(out)
}

generate_data_spec_f <- simulatr_function(f = generate_data_f,
                                          arg_names = formalArgs(generate_data_f), loop = FALSE)

# 4. create the method functions
# a. OLS regression
# Note: y is the response vector of length n,
# the output is a data frame with columns "parameter," "target," "value"
ols_f <- function(y, design_matrix) {
  library(magrittr)
  fit_ols <- lm(y ~ design_matrix)
  broom::tidy(fit_ols) %>% dplyr::rename() %>%
  dplyr::select(term, estimate, std.error) %>%
  dplyr::rename(parameter = term, std_error = std.error) %>%
  tidyr::pivot_longer(cols = c("estimate", "std_error"), names_to = "target") %>%
  dplyr::mutate(parameter = factor(x = parameter,
                                   levels = c("(Intercept)", "design_matrix1", "design_matrix2", "design_matrix3", "design_matrix4"),
                                   labels = c("intercept", "beta_1", "beta_2", "beta_3", "beta_4")))
}
ols_spec_f <- simulatr_function(f = ols_f, arg_names = "design_matrix", loop = TRUE)

# b. lasso regression
lasso_f <- function(y, design_matrix) {
  library(magrittr)
  fit_lasso <- glmnet::cv.glmnet(x = design_matrix, y = y)
  beta_hat <- glmnet::coef.glmnet(object = fit_lasso, "lambda.min")[,1] %>% setNames(., NULL)
  data.frame(parameter = c("intercept", "beta_1", "beta_2", "beta_3", "beta_4"),
             target = "estimate", value = beta_hat)
}
lasso_spec_f <- simulatr_function(f = lasso_f, arg_names = "design_matrix", loop = TRUE)
method_list <- list(ols = ols_spec_f, lasso = lasso_spec_f)

sim_spec <- simulatr_specifier(parameter_grid = p_grid, fixed_parameters = fixed_params,
                   generate_data_function = generate_data_spec_f, run_method_functions = method_list)

check <- check_simulatr_specifier_object(simulatr_spec = sim_spec, B_in = 3, parallel = TRUE)
