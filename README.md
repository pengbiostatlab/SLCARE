
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SLCARE

<!-- badges: start -->
<!-- badges: end -->

Recurrent event data frequently arise in biomedical follow-up studies.
The concept of latent classes enables researchers to characterize
complex population heterogeneity in a plausible and parsimonious way.
SLCARE implements a robust and flexible algorithm to carry out Zhao et
al.(2022)’s latent class analysis method for recurrent event data, where
semiparametric multiplicative intensity modeling is adopted. SLCARE
returns estimates for non-functional model parameters along with the
associated variance estimates. Visualization tools are provided to
depict the estimated functional model parameters and related functional
quantities of interest. SLCARE also delivers a model checking plot to
help assess the adequacy of the fitted model.

## Installation

You can install the development version of SLCARE like so:

``` r
if (!require("pak", quietly = TRUE))
    install.packages("pak")

pak::pak("qyxxx/SLCARE")
```

Or install SLCARE from CRAN with:

``` r
install.packages("SLCARE")
```
