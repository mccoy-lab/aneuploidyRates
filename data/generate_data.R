#' This file runs the aneuploidyRates package and writes
#' the generated data to a csv file
#'
source("R/find_rates.R")

# find_rates(num.trials = 1000, tolerance = 0.01)
# system.time(
#   df <- find_rates_faster(num.trials = 50)
# )
# print(typeof(df))
# print(df)

df1 <- find_rates_faster(num.trials = 50)
df2 <- find_rates(num.trials = 50)
microbenchmark(df1, df2, times = 5)
