#' Takes a biopsy of an embryo object.
#'
#' @description
#' Return an int corresponding to the biopsy types:
#' 2 = aneuploid
#' 1 = mosaic
#' 0 = euploid
#'
#' @param em the embryo for the biopsy sampling
#' @param biop.size the number of cells to be sampled
#'
#' @return an integer representing the biopsy type
#' @export
#'
#' @examples
#' take_biopsy(tessera::Embryo(
#' n.cells = 100,
#' n.chrs = 1,
#' prop.aneuploid = 0.5,
#' dispersal = 0), biop.size =5)
#'
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
