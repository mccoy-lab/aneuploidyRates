#' This file runs through a range of meiotic and mitotic probabilities and
#' select the most fitting rates that deduces the real-life data.
#'
#' Data for comparison:
#' From Viotti et al. 2021 (https://doi.org/10.1016/j.fertnstert.2020.11.041),
#' we leveraged their summary statistics presented in Figure 1A and calculated
#' a weighted average to determine percentage of aneuploidy, mosaicism, and euploidy
#' biopsies across 5 clinics.
#'
#' A total of 73218 embryos are collected, of which 38.8% (28,431) is euploid,
#' 18.6% (13,602) is mosaic, and 42.6% (31,185) is aneuploid.

#'
#' (return range or data points?)
#' The selection process will be done by EasyABC
library(EasyABC)
source("R/summarize_biopsy.R")

#' Return two ranges of the error probabilities
#'
#'@param meio.range a double for the uniform distribution range to generate a meiotic error rate
#'@param mito.range a double for the uniform distribution range to generate a mitotic error rate
#'@param num.trials the number of trials to run the simulation. Each trial
#'generates a data point with the two error rates.
#'
#'@return the list of data points that will generate
#'
find_rates <- function(meio.range = c(0, 1),
                       mito.range = c(0, 1),
                       num.trials = 100) {
  # Error messages
  if(length(meio.range) != 2 | length(mito.range) != 2){
    stop(paste0("Must input a range (e.g. (0,1)) for the error ranges"))
  }
  if (meio.range[1] < 0 | mito.range[1] < 0) {
    stop(paste0(
      "The probabilities: ",
      meio.range[1],
      ", ",
      mito.range[1],
      " must be at least 0"
    ))
  }
  if (meio.range[2] > 1 | mito.range[2] > 1) {
    stop(paste0(
      "The probabilities: ",
      meio.range[2],
      ", ",
      mito.range[2],
      " must be at most 1"
    ))
  }
  if (num.trials %% 1 != 0) {
    stop(paste0(
      "The number of trials for the simulation: ",
      num.trials,
      "should be an integer"
    ))
  }

  # Set the model
  rates_model <- function(probs) {
    summarize_biopsy(meio = probs[1],
                     mito = probs[2])[1,3:5]
  }
  # Choose the distribution to draw input
  rates_prior <- list(c("unif", meio.range[1], meio.range[2]),
                      c("unif", mito.range[1], mito.range[2]))
  tolerance = 0.05
  # The expected value: euploid-0.388, aneu-0.186, mosaic-0.426
  sum_stat_obs = list(0.388, 0.186, 0.426)

  rates_sim <-
    ABC_rejection(
      model = rates_model,
      prior = rates_prior,
      nb_simul = num.trials,
      summary_stat_target = sum_stat_obs,
      tol = tolerance
    )
  print(rates_sim)

  # Set up return format
  result <- cbind(rates_sim$param,rates_sim$stats)
  colnames(result) <- c("prob.meio", "prob.mito","euploid", "mosaic", "aneuploid")
  return(result)
}

