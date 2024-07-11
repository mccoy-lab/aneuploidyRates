#' Returns a biopsy summary for a set of meiotic and mitotic error rates and dispersal.
#'
#' A wrapper function for generating the biopsy results, it counts the proportions
#' of euploid, mosaic, and aneuploid biopsy results out of sampling a batch of
#' embryos with the same error rates and dispersal.
#'
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
#'
#' @examples
#' summarize_biopsy(
#'   meio = 0,
#'   mito = 0.3,
#'   dispersal = 0.5,
#'   hide.default.param = TRUE
#' )
#' summarize_biopsy(
#'   num.em = 200,
#'   meio = 0.3,
#'   mito = 0.02,
#'   num.cell = 200,
#'   num.chr = 1,
#'   dispersal = 0,
#'   concordance = 0,
#'   hide.default.param = FALSE
#' )
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
    stop(paste0("The number of embryos: ",
                num.em,
                "should be an integer"))
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
  # c(0, meio, mito, dispersal, 0, 0, 0)
  result <- matrix(nrow = num.em, ncol = 7)
  euploid <- 0
  mosaic <- 0
  aneuploid <- 0

  result[,2] <- meio
  result[,3] <- mito
  result[,4] <- dispersal

  if (!hide.default.param) {
    # Keep the other parameters used for constructing the embryo
    result <- cbind(result, matrix(nrow = num.em, ncol = 3))
    result[,8] <- num.cell
    result[,9] <- num.chr
    result[,10] <- concordance
  }

  # Generate embryos and collect biopsy results
  for (i in 1:num.em) {
    # Convert to prop.aneu (can differ with the same error rates due to the
    # conversion simulation)
    prop.aneu <- prob_to_prop(prob.meio = meio, prob.mito = mito)
    # Sum up all prop.aneus (will take the average later)
    result[i, 1] <- prop.aneu

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
      euploid <- euploid + 1

    } else if (type == 1) {
      mosaic <- mosaic + 1

    } else{
      aneuploid <- aneuploid + 1

    }
  }

  # Calculate the average prop.aneu and convert the types to percentages
  result[,5] <- euploid / num.em
  result[,6] <- mosaic / num.em
  result[,7] <- aneuploid / num.em
  return(result)
}
