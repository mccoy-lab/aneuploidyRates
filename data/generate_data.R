#' This file runs the aneuploidyRates package and writes
#' the generated data to a csv file
#'
source("R/find_rates.R")
library(microbenchmark)

# find_rates(num.trials = 1000, tolerance = 0.01)

# Performance check
# system.time(
#   df <- find_rates_faster(num.trials = 50)
# )
# print(typeof(df))
# print(df)

# system.time(
#   df1 <- find_rates_faster(num.trials = 50)
# )
# system.time(
#   df2 <- find_rates(num.trials = 50)
# )
# microbenchmark(df1, df2, times = 10000)

