# This file runs the aneuploidyRates package
#
# source("R/find_rates.R")
if(!require(aneuploidyRates)){
  if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org")
  library(devtools)
  install_github("mccoy-lab/aneuploidyRates")
}
library(aneuploidyRates)
find_rates(num.trials = 1000, disp.range = list(1,1), tolerance = 0.01)
# find_rates(num.trials = 30, tolerance = 1)
