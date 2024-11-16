# This file runs the aneuploidyRates package and writes the generated data to a csv file
#
# source("R/find_rates.R")
if(!require(aneuploidyRates)){
  if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org")
  library(devtools)
  install_github("mccoy-lab/aneuploidyRates")
}
library(aneuploidyRates)
find_rates(num.trials = 1000, expected = c(0.232, 0.187, 0.581), disp.range = list(1,1), tolerance = 0.01)
