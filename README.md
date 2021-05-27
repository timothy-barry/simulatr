
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simulatr

Simulation studies play a crucial role in the development and assessment
of statistical methods. However, writing portable and scalable
simulation code can be challenging. `Simulatr` decouples the
*specification* of a simulation from the *execution* of a simulation,
enabling users to develop a simulation study on their laptop and
seamlessly run the simulation at-scale on a distributed computing
platform. Supported platforms include high-performance computing
clusters and cloud computing services (e.g., Microsoft Azure, Amazon Web
Services, etc.).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("timothy-barry/simulatr")
```
