
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epicprior

The goal of `epicprior` is to provide a simple Shiny app for specifying
statistical distributions based on input from domain experts. These
distributions can be used in Bayesian prior models or for simulating
data. The basic idea is to provide thresholds that separate reasonable
from extreme values, based on recomendations from [Michael
Betancourt](https://betanalpha.github.io/writing/). This Shiny app
includes functions from the `fitdistrplus` package to derive parameters
for distributions for which the specified thresholds are at specific
quantiles. The app also contains detailed instructions.

## Installation

You can install the development version of epicprior from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("forsterepi/epicprior")
```

## Usage

Run `collect()` to launch the Shiny app and read the instructions on the
first tab.

``` r
# epicprior::collect()
```
