# This file runs the aneuploidyRates package and writes the generated data to a csv file
#
# source("R/find_rates.R")
if(!require(aneuploidyRates)){
  if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org")
  library(devtools)
  install_github("mccoy-lab/aneuploidyRates")
}
library(aneuploidyRates)
find_rates(meio.range = list(0.35, 0.51), mito.range = list(0.019, 0.025), num.trials = 1000, tolerance = 0.01)
# find_rates(num.trials = 50)

# Performance check
# system.time(
#   df <- find_rates(num.trials = 50)
# )
# print(typeof(df))
# print(df)
