#' This file runs through a range of meiotic and mitotic probabilities and dispersal,
#' and select the most fitting values that deduces the expected data.

#' Data for comparison:
#' From Viotti et al. 2021 (https://doi.org/10.1016/j.fertnstert.2020.11.041),
#' we leveraged their summary statistics presented in Figure 1A and calculated
#' a weighted average to determine percentage of aneuploidy, mosaicism, and euploidy
#' biopsies across 5 clinics.
#'
#' A total of 73218 embryos are collected, of which 38.8% (28,431) is euploid,
#' 18.6% (13,602) is mosaic, and 42.6% (31,185) is aneuploid.

#' The selection process will be done by EasyABC
# install.packages("EasyABC")
if(!require(EasyABC)) {
  # EasyABC dependency
  if(!require(locfit)) install.packages("locfit", repos = "http://cran.us.r-project.org")
  if(!require(nnet)) install.packages("nnet", repos = "http://cran.us.r-project.org")
  if(!require(lattice)) install.packages("lattice", repos = "http://cran.us.r-project.org")
  if(!require(survival)) install.packages("survival", repos = "http://cran.us.r-project.org")
  if(!require(Matrix)) install.packages("Matrix", repos = "http://cran.us.r-project.org")
  if(!require(MASS)) install.packages("MASS", repos = "http://cran.us.r-project.org")
  if(!require(quantreg)) install.packages("quantreg", repos = "http://cran.us.r-project.org")

  install.packages("EasyABC", repos = "http://cran.us.r-project.org")
}
library(EasyABC)
# source("R/summarize_biopsy.R")

#' Return a data frame of the selected error probabilities and dispersal
#'
#'@param meio.range a double for the uniform distribution range to generate a meiotic error rate
#'@param mito.range a double for the uniform distribution range to generate a mitotic error rate
#'@param disp.range a double for the uniform distribution range to generate the extent of dispersal
#'@param expected   a list of ratios derived from published data, used for selecting
#'the fitting error rates and dispersal
#'@param tolerance  the percent of simulations to be kept near the expected values
#'@param num.trials the number of trials to run the simulation. Each trial
#'@param hide.param a boolean to show/hide the constant default parameters: num.cells,
#'num.chr, concordance
#'@export
#'
#'@return a data frame of the corresponding embryo (prop.aneu), the selected
#'error rate pair, its dispersal, and biopsy information.
#'
find_rates <- function(meio.range = list(0, 1),
                       mito.range = list(0, 1),
                       disp.range = list(0, 1),
                       expected = c(0.388, 0.186, 0.426),
                       tolerance = 0.05,
                       num.trials = 100,
                       hide.param = TRUE) {
  # Error messages
  if (length(meio.range) != 2 |
      length(mito.range) != 2 | length(disp.range) != 2) {
    stop(paste0(
      "Must input a range (e.g. (0,1)) for the error and dispersal ranges"
    ))
  }
  if (meio.range[1] < 0 | mito.range[1] < 0 | disp.range[1] < 0) {
    stop(
      paste0(
        "The probabilities: ",
        meio.range[[1]],
        ", ",
        mito.range[[1]],
        ", and dispersal: ",
        disp.range[[1]],
        " must be at least 0"
      )
    )
  }
  if (meio.range[2] > 1 | mito.range[2] > 1 | disp.range[2] > 1) {
    stop(
      paste0(
        "The probabilities: ",
        meio.range[[2]],
        ", ",
        mito.range[[2]],
        ", and dispersal: ",
        disp.range[[2]],
        " must be at most 1"
      )
    )
  }
  if (num.trials %% 1 != 0) {
    stop(
      paste0(
        "The number of trials for the simulation: ",
        num.trials,
        "should be an integer"
      )
    )
  }
  if (tolerance <= 0 | tolerance > 1) {
    stop(paste0("The tolerance: ",
                tolerance,
                " should be in the range (0, 1]"))
  }
  if (sum(expected) != 1) {
    stop(paste0(
      "The expected percentages of all three embryo types should
                sum up to 1"
    ))
  }

  # Set up matrix for later output
  remaining.data <- matrix(ncol = 7)
  if (!hide.param) {
    remaining.data <- cbind(remaining.data, matrix(ncol = 3))
  }

  # Set the model for biopsy summary
  rates_model <- function(probs) {
    biopsy <- summarize_biopsy(
      meio = probs[[1]],
      mito = probs[[2]],
      dispersal = probs[[3]],
      hide.default.param = hide.param
    )

    # Saves all datat (used for displaying prop.aneu and other default params later)
    remaining.data <<- rbind(remaining.data, biopsy)
    # Returns only the biopsy types
    return(biopsy[5:7])
  }

  # Choose the distribution to draw inputs. Assume uniform distributions.
  rates_prior <- list(
    c("unif", meio.range[[1]], meio.range[[2]]),
    c("unif", mito.range[[1]], mito.range[[2]]),
    c("unif", disp.range[[1]], disp.range[[2]])
  )

  # Feed into ABC_rejection
  rates_sim <-
    ABC_rejection(
      # previously set biopsy model, returns a list of biopsy type percentages
      model = rates_model,
      # previously set distributions for error rates and dispersal inputs
      prior = rates_prior,
      # number of simulations
      nb_simul = num.trials,
      # expected values, used for selecting results
      summary_stat_target = expected,
      # percentage of closest results to be selected
      tol = tolerance,
      use_seed = TRUE,
      n_cluster = 5,
      progress_bar = TRUE
    )

  # Set up return format: from the saved data, select the rows with ABC_rej's
  # returned parameters
  result <-
    remaining.data[remaining.data[, 2] %in% rates_sim$param[, 1]
                   &
                     remaining.data[, 3] %in% rates_sim$param[, 2], ]

  # Set row names (numbers)
  rownames(result) <- 1:nrow(result)

  # Set column names
  if (hide.param) {
    colnames(result) <-
      c(
        "prop.aneu",
        "prob.meio",
        "prob.mito",
        "dispersal",
        "euploid",
        "mosaic",
        "aneuploid"
      )
  } else{
    colnames(result) <-
      c(
        "prop.aneu",
        "prob.meio",
        "prob.mito",
        "dispersal",
        "euploid",
        "mosaic",
        "aneuploid",
        "num.cell",
        "num.chr",
        "concordance"
      )
  }

  return(data.frame(result))
}
