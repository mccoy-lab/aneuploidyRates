
# aneuploidyRates

<!-- badges: start -->
<!-- badges: end -->

The goal of aneuploidyRates is to infer the error rates of early human embryo development based on the in vitro fertilization (IVF) clinic biopsy data. Using an approximate Bayesian computation approach that identifies the parameters with best-matching results to the observed data, this package investigates the rate of meiotic and mitotic error as well as the dispersal of aneuploid cells within the embryo. 

## Installation

You can install the development version of aneuploidyRates from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools", repos = "http://cran.us.r-project.org")
devtools::install_github("mccoy-lab/aneuploidyRates")
```

## Example

This is a basic example which shows you how to deduce the most fitting 3 pairs of error rates from 50 trials of simulation:

``` r
library(aneuploidyRates)
## basic example code
find_rates(num.trials = 50)
```

Note that the above example runs 50 trials of simulations, which randomly selects 50 sets of parameters (pairs of error rates and dispersals) within the approximate Bayesian computation model. Each pair of error rate is converted to the proportion of aneuploidy (prop.aneu) cells 100 times and creates 100 embryos. They are very likely to have unique prop.aneu, but all 100 embryos within the same set share the same meiotic and mitotic error rates and dispersal. 

Overall, a 50-trial simulation creates 5,000 embryos and, with a tolerance of 0.05, selects the 3 sets of parameters that deduces the closest result to the referencing data.
