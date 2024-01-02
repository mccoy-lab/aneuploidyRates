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

summarize_biopsy <- function(num.em = 100,
                             meio,
                             mito,
                             num.cell = 200,
                             num.chr = 1,
                             dispersal = 0,
                             concordance = 0,
                             hide.default.param = TRUE) {
  # Error messages
  if (meio < 0 | mito < 0) {
    stop(paste0("The probabilities: ",
                meio,
                ", ",
                mito,
                " must be at least 0"))
  }
  if (meio > 1 | mito > 1) {
    stop(paste0("The probabilities: ",
                meio,
                ", ",
                mito,
                " must be at most 1"))
  }
  if (num.em %% 1 != 0) {
    stop(paste0(
      "The number of cell division: ",
      num.division,
      "should be an integer"
    ))
  }
  if (num.em < 0) {
    stop(paste0("The number of embryos: ",
                num.em,
                " must be at least 0"))
  }

  # set up result file
  # without the default settings
  # result <- data.frame(
  #   prop.aneu = 0,
  #   prob.meio = meio,
  #   prob.mito = mito,
  #   dispersal = 0,
  #   euploid = 0,
  #   mosaic = 0,
  #   aneuploid = 0
  # )
  result <- c(0, meio, mito, dispersal, 0, 0, 0)
  if (!hide.default.param) {
    result <- c(result, num.cell, num.chr, concordance)
  }
  # print(result)

  # run embryos
  for (i in 1:num.em) {
    # convert to prop.aneu
    prop.aneu <- prob_to_prop(prob.meio = meio, prob.mito = mito)
    # store the sum of all prop.aneus (will take the average later)
    result[1] <- result[1] + prop.aneu

    # create an embryo
    em <- create_embryo(
      prop.aneuploid = prop.aneu,
      n.cell = num.cell,
      n.chr = num.chr,
      dispersal = dispersal,
      concordance = concordance
    )

    # take biopsy
    type <- take_biopsy(em)

    # add to result
    if (type == 0) {
      result[5] <- result[5] + 1

    } else if (type == 1) {
      result[6] <- result[6] + 1

    } else{
      result[7] <- result[7] + 1

    }
  }

  # Add dispersal as a potential parameter
  # calculate the average prop.aneu and convert the types to percentages
  result[c(1, 5:7)] <- result[c(1, 5:7)] / num.em
  return(result)
}

result <- summarize_biopsy(
  meio = 0,
  mito = 0.3,
  dispersal = 0.5,
  hide.default.param = TRUE
)
# print(result[c(1, 5:7)] == list(1, 0, 0, 1))
# print(result)
# print(typeof(result))
