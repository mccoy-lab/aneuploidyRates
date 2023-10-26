# This file returns a data frame of a summary of multiple trials
# (Will this file be doing too much jobs by calling all the previous functions?)

source("R/create_embryo.R")
source("R/prob_to_prop.R")
source("R/take_biopsy.R")

#' A wrapper function for generating the biopsy results
#' @param num.em the number of embryos to be created
#' @param meio the probability of having a meiotic error
#' @param mito the probability of having a meiotic error
#'
#' @return a single-row list with columns "prob.meio", "prob.mito", and the three
#' biopsy types shown as percentages.
#'

summarize_biopsy <- function(num.em = 100, meio, mito) {
  # Error messages
  if (meio < 0 | mito < 0) {
    stop(paste0(
      "The probabilities: ",
      meio,
      ", ",
      mito,
      " must be at least 0"
    ))
  }
  if (meio > 1 | mito > 1) {
    stop(paste0(
      "The probabilities: ",
      meio,
      ", ",
      mito,
      " must be at most 1"
    ))
  }
  if (num.em %% 1 != 0) {
    stop(paste0(
      "The number of cell division: ",
      num.division,
      "should be an integer"
    ))
  }
  if(num.em < 0){
    stop(paste0(
      "The number of embryos: ",
      num.em,
      " must be at least 0"
    ))
  }

  # set up result file
  result <- data.frame(
    prop.aneu = 0,
    prob.meio = meio,
    prob.mito = mito,
    euploid = 0,
    mosaic = 0,
    aneuploid = 0
  )
  # run embryos
  for (i in 1:num.em) {
    # convert to prop.aneu
    prop.aneu <- prob_to_prop(prob.meio = meio, prob.mito = mito)
    result$prop.aneu <- prop.aneu
    # create an embryo
    em <- create_embryo(prop.aneuploid = prop.aneu)
    # take biopsy
    type <- take_biopsy(em)
    # add to result
    if (type == 0) {
      result$euploid <- result$euploid + 1

    } else if (type == 1) {
      result$mosaic <- result$mosaic + 1

    } else{
      result$aneuploid <- result$aneuploid + 1

    }
  }

  # convert the types to percentages
  result <- cbind(result[, 1:3], result[, 4:6] / num.em)
  return(result)
}
#
# result = summarize_biopsy(meio = 1, mito = 0.5)
# print(result[1,4:6] == list(0,0,1))
# print(result)
