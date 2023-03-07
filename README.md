
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `simulatr`: write portable and scalable simulation code

Simulation studies play a crucial role in the development and assessment
of statistical methods. However, writing portable and scalable
simulation code is challenging. With the help of
[Nextflow](https://nextflow.io/), `simulatr` decouples the
*specification* of a simulation from the *execution* of a simulation,
enabling users to develop a simulation study on their laptop and then
seamlessly run the simulation at-scale on a distributed computing
platform. Supported platforms will include high-performance computing
clusters and cloud computing services (e.g., Microsoft Azure, Amazon Web
Services, etc.). Please visit the
[`simulatr-pipeline`](https://github.com/timothy-barry/simulatr-command-line)
repository to download the associated `simulatr` Nextflow pipeline. Note
the `simulatr` allows simulations to be run directly in RStudio as well.

Please see the
[tutorial](https://katsevich-lab.github.io/simulatr/articles/intro-to-simulatr.html)
to get started.

**Note**: This package is experimental and under active development. Use
with caution.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("katsevich-lab/simulatr")
