utils::globalVariables(c("target", "value", "confint_lower", "confint_higher", "grid_row_id", "parameter", "method"))

#' simulatr: A package that powers portable and scalable simulation studies
#'
#' Simulation studies play a crucial role in the development and assessment of statistical methods. However, writing portable and scalable simulation code can be challenging. Simulatr decouples the *specification* of a simulation from the *execution* of a simulation, enabling users to develop a simulation study on their laptop and seamlessly run the simulation at-scale on a distributed computing platform. Supported platforms include high-performance computing clusters and cloud computing services (e.g., Microsoft Azure, Amazon Web Services, etc.).
#' @importFrom magrittr %>%
#' @import methods
#' @docType package
#'
#' @name simulatr
NULL
