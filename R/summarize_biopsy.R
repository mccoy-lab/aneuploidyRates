# This file returns a data frame of a summary of multiple trials
# (Will this file be doing too much jobs by calling all the previous functions?)

source("R/create_embryo.R")
source("R/prob_to_prop.R")
source("R/take_biopsy.R")

#' A wrapper function for generating the biopsy results
#' @param num.em the number of embryos to be created
#' @param meio the probability of having a meiotic error
#' @param mito the probability of having a meiotic error
#' (try with probability ranges instead of )
summarize_biopsy <- function(num.em = 1000, meio, mito){
  # convert probs to prop.aneu
  # create embryos
  # take biopsies
  # summarize the result

  # set up result file
  result = data.frame(matrix(ncol = 5, nrow = num.em))
  colnames(sum) <-
    c("prob.meio", "prob.mito", "euploid", "mosaic", "aneuploid")
  euploid <- 0
  mosaic <- 0
  aneuploid <- 0
  for(i in 1:num.em){
    # convert to prop.aneu
    prop.aneu <- prob_to_prop(prob.meio = meio, prob.mito = mito)
    # create an embryo
    em <- create_embryo(prop.aneuploid = prop.aneu)
    print(prop.aneu)
    # take biopsy
    type <- take_biopsy(em)
    # add to result
    if(type == 0){
      euploid = euploid + 1;
    }else if(type == 1){
      mosaic = mosaic + 1;
    }else{
      aneuploid = aneuploid + 1;
    }
  }
    result <- c(meio, mito, euploid, mosaic, aneuploid);

  print(result)
  return(result)
  }


