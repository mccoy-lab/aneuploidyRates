#' This file takes the biopsy of an embryo object.


#' Take a biopsy of the given embryo, return an int corresponding to the biopsy types
#' 2 = aneuploid
#' 1 = mosaic
#' 0 = euploid
#' @param em the embryo for the biopsy sampling
#' @param biop.size the number of cells to be sampled
#' @export
#' @return an integer representing the biopsy type

take_biopsy <- function(em, biop.size = 5) {
  # error messages
  if (biop.size <= 0 || biop.size > nrow(em@ploidy)) {
    stop(paste(
      "Biopsy size (",
      biop.size,
      ") must be greater than 0 and at most",
      nrow(em@ploidy)
    ))
  }

  biopsy.result <-
    tessera::takeBiopsy(embryo = em, biopsy.size = biop.size)

  # assign types based on results
  if (biopsy.result == 0) {
    # no aneuploid cells -- an euploid embryo
    return(0)
  } else if (biopsy.result == biop.size) {
    # all aneuploid cells -- an aneuploid embryo
    return(2)
  } else{
    # mosaic
    return(1)
  }
}
