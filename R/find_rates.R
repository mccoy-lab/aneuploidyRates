

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
# install.packages("EasyABC")

library(EasyABC)
source("R/summarize_biopsy.R")

#' Return two ranges of the error probabilities
#'
#'@param meio.range a double for the uniform distribution range to generate a meiotic error rate
#'@param mito.range a double for the uniform distribution range to generate a mitotic error rate
#'@param expected   a list of ratios derived from published data, used for selecting
#'the fitting error rates
#'@param tolerance  the percent of simulations to be kept near the expected values
#'@param num.trials the number of trials to run the simulation. Each trial
#'@param hide.param a boolean to show/hide the constant default parameters: num.cells,
#'num.chr, dispersal, concordance
#'generates a data point with the two error rates.
#'
#'@return the list of data points that will generate
#'
find_rates <- function(meio.range = list(0, 1),
                       mito.range = list(0, 1),
                       expected = c(0.388, 0.186, 0.426),
                       tolerance = 0.05,
                       num.trials = 100,
                       hide.param = TRUE) {
  # Error messages
  if(length(meio.range) != 2 | length(mito.range) != 2){
    stop(paste0("Must input a range (e.g. (0,1)) for the error ranges"))
  }
  if (meio.range[1] < 0 | mito.range[1] < 0) {
    stop(paste0(
      "The probabilities: ",
      meio.range[[1]],
      ", ",
      mito.range[[1]],
      " must be at least 0"
    ))
  }
  if (meio.range[2] > 1 | mito.range[2] > 1) {
    stop(paste0(
      "The probabilities: ",
      meio.range[[2]],
      ", ",
      mito.range[[2]],
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
  if (tolerance <= 0 | tolerance > 1) {
    stop(paste0(
      "The tolerance: ",
      tolerance,
      " should be in the range (0, 1]"
    ))
  }
  if(sum(expected) != 1){
    stop(paste0("The expected percentages of all three embryo types should
                sum up to 1"))
  }

   remaining.data <- data.frame(prop.aneu = NULL);

  # Set the model
  rates_model <- function(probs) {
    biopsy <- summarize_biopsy(meio = probs[[1]],
                     mito = probs[[2]],
                     hide.default.param = hide.param)
    # print(biopsy)
    if(hide.param){
    remaining.data <<- rbind(remaining.data, biopsy[,1:3])
    }else{ # keep track of the hidden parameters
      remaining.data <<- rbind(remaining.data, biopsy[,c(1:3, 7:10)])
    }
    # print(remaining.data$prob.mito)
    return(biopsy[1, 4:6])
  }

  # Choose the distribution to draw input
  rates_prior <- list(c("unif", meio.range[[1]], meio.range[[2]]),
                      c("unif", mito.range[[1]], mito.range[[2]]))

  rates_sim <-
    ABC_rejection(
      model = rates_model,
      prior = rates_prior,
      nb_simul = num.trials,
      summary_stat_target = expected,
      tol = tolerance
    )
  # print(rates_sim)
  # print(remaining.data)


  # Set up return format
  if(hide.param){
    remaining.data <- remaining.data[remaining.data$prob.meio %in% rates_sim$param[,1]
                                     & remaining.data$prob.mito %in% rates_sim$param[,2],1]
    result <- cbind(remaining.data, rates_sim$param,rates_sim$stats)
    rownames(result) <- 1:nrow(result)
    colnames(result) <- c("prop.aneu","prob.meio", "prob.mito","euploid", "mosaic", "aneuploid")
  }
  else{ # Display the hidden default parameters
    remaining.data <- cbind(remaining.data[remaining.data$prob.meio %in% rates_sim$param[,1]
                                     & remaining.data$prob.mito %in% rates_sim$param[,2],])
    result <- cbind(remaining.data[,1],rates_sim$param,rates_sim$stats, remaining.data[,4:7])
    rownames(result) <- 1:nrow(result)
    colnames(result) <- c("prop.aneu","prob.meio", "prob.mito","euploid", "mosaic"
                          , "aneuploid", "num.cell", "num.chr", "dispersal", "concordance")
  }
  return(data.frame(result))
}


# hist(test$prob.meio)
# hist(test$prob.mito)
# prop.aneu prob.meio  prob.mito euploid mosaic aneuploid
# 1 0.4776953 0.3437730 0.02664748    0.38   0.25      0.37
# 2 0.7347656 0.5536075 0.04644948    0.17   0.18      0.65
# 3 0.5870703 0.2700669 0.06577964    0.27   0.38      0.35
# 4 0.4724609 0.2074682 0.05325346    0.33   0.38      0.29
# 5 0.7059375 0.3082673 0.09855301    0.11   0.34      0.55
