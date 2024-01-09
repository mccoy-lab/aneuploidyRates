# This file returns a vector of a biopsy summary for a batch of embryos with the
# same meiotic and mitotic error rates.

# source("R/create_embryo.R")
# source("R/prob_to_prop.R")
# source("R/take_biopsy.R")


#' A wrapper function for generating the biopsy results
#' @param num.em the number of embryos to be created
#' @param meio the probability of having a meiotic error
#' @param mito the probability of having a meiotic error
#' @param num.cell the number of cells in the embryo
#' @param num.chr the number of chromosome pairs per cell
#' @param dispersal the dispersion vector of the aneuploid cells (0-1)
#' @param concordance the concordance between aneuploid cells for each chromosome (0-1).
#' @param hide.default.param the boolean to exclude other parameters used in
#' constructing the embryo: num.cell, num.chr, concordance. If false, the returned
#' vector will contain these values.
#'
#' @return a vector with columns: "prop.aneu", "prob.meio", "prob.mito", "dispersal",
#' and the three biopsy types shown as percentages.
#' @export

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
      "The number of embryos: ",
      num.em,
      "should be an integer"
    ))
  }
  if (num.em < 0) {
    stop(paste0("The number of embryos: ",
                num.em,
                " must be at least 0"))
  }

  # Set up result file, filing in the inputs
  # result <- c(
  #   prop.aneu,
  #   prob.meio,
  #   prob.mito,
  #   dispersal,
  #   euploid,
  #   mosaic,
  #   aneuploid
  # )
  result <- c(0, meio, mito, dispersal, 0, 0, 0)
  if (!hide.default.param) {
    # Keep the other parameters used for constructing the embryo
    result <- c(result, num.cell, num.chr, concordance)
  }

  # Generate embryos and collect biopsy results
  for (i in 1:num.em) {
    # Convert to prop.aneu (can differ with the same error rates due to the
    # conversion simulation)
    prop.aneu <- prob_to_prop(prob.meio = meio, prob.mito = mito)
    # Sum up all prop.aneus (will take the average later)
    result[1] <- result[1] + prop.aneu

    # Create an embryo
    em <- create_embryo(
      prop.aneuploid = prop.aneu,
      n.cells = num.cell,
      n.chrs = num.chr,
      dispersal = dispersal,
      concordance = concordance
    )

    # Take one biopsy of this embryo
    type <- take_biopsy(em)

    # Add its type to categories in result
    if (type == 0) {
      result[5] <- result[5] + 1

    } else if (type == 1) {
      result[6] <- result[6] + 1

    } else{
      result[7] <- result[7] + 1

    }
  }

  # Calculate the average prop.aneu and convert the types to percentages
  result[c(1, 5:7)] <- result[c(1, 5:7)] / num.em
  return(result)
}

# result <- summarize_biopsy(
#   meio = 0,
#   mito = 0.3,
#   dispersal = 0.5,
#   hide.default.param = TRUE
# )
# print(result[c(1, 5:7)] == list(1, 0, 0, 1))
# print(result)
# print(typeof(result))


# system.time(
#   df2 <- summarize_biopsy(
#     meio = 0,
#     mito = 0.3,
#     dispersal = 0.5,
#     hide.default.param = TRUE
#   )
# )
