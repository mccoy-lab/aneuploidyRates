#' This file runs the aneuploidyRates package and writes
#' the generated data to a csv file
#'
source("R/find_rates.R")

find_rates(num.trials = 1000, tolerance = 0.01)
