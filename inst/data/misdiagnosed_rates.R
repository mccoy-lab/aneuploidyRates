# This file evaluates the percentages of mosaic embryos if, by a certain ratio,
# some of the mosaic biopsies are actually euploid/aneuploid polluted by noises

if(!require(aneuploidyRates)){
  if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org")
  library(devtools)
  install_github("mccoy-lab/aneuploidyRates")
}
library(aneuploidyRates)
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
library(dplyr)
# expected = c(0.388, 0.186, 0.426)
# # Set number of steps -- number of times to run find_rates
# steps = 10
# # calculate each increment
# incr = 0.186/10
#
# datalist = vector("list", length = steps)
# for (i in 1:steps) {
#   df <- find_rates(num.trials= 1000,)
# }

args <- commandArgs(trailingOnly = TRUE)
rate <- strtoi(args[1]) - 1

# set number of steps
steps = 10
incr = 0.186/10
expected_values = c((0.388 + rate * (incr / 2)),
                (0.186 - rate *incr),
                (0.426 + rate *(incr/2)))
df <- find_rates(num.trials = 1000, expected = expected_values,
           tolerance = 0.01)
df %>% mutate(misdiagnosed.rates = (1/steps * rate))
