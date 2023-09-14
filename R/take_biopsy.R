#' This file takes the biopsy of an embryo object.
library(tessera)

# so if I'm directly calling Tessera, how do I keep this function in the file?
# Do I still need a file for this function?

# Alternative: copy and paste the desired function from Tessera's source code


#' Take a sample from an embryo
#'
#' The cell at the given index is taken,
#' plus the closest n neighbouring cells where n = n.sampled.cells-1.
#'
#' @param embryo an embryo
#' @param biopsy.size the number of cells to biopsy
#' @param index.cell the index of the cell to begin biopsying. Must be a value
#'  between 1 and \code{nrow(embryo)}
#'  @param chromosome the chromosome to test
#'
#' @return the number of aneuploid cells in the biopsy
#'
#' @examples
# e <- Embryo()
# takeBiopsy(e, 5, 1)
setGeneric(
  name = "takeBiopsy",
  def = function(embryo, ...) {
    standardGeneric("takeBiopsy")
  }
)


setMethod("takeBiopsy", signature = "Embryo", function(embryo,
                                                       biopsy.size = 5,
                                                       index.cell = 1,
                                                       chromosome = 0) {
  if (index.cell < 1 | index.cell > length(embryo@x)) {
    stop(paste(
      "index.cell (",
      index.cell,
      ") must be between 1 and",
      length(embryo@x)
    ))
  }

  if (chromosome < 0 | chromosome > ncol(embryo@ploidy)) {
    stop(paste(
      "Chromosome (",
      chromosome,
      ") must be between 0 and",
      ncol(embryo@ploidy)
    ))
  }

  # Get the distance list for the index cell
  sample.list <- embryo@dists[[paste0("d", index.cell)]]

  # Choose the cells to join the biopsy based on distance
  isSampled <-
    embryo@dists[[paste0("d", index.cell)]] <= max(head(sort(sample.list), n = biopsy.size))

  # count all chromsomes; don't care which chromosome is aneuploid
  # just is aneuploid or is not aneuploid
  # print(embryo@ploidy)
  if (chromosome == 0) {
    return(sum(embryo@ploidy[isSampled,] != embryo@euploidy)) # the total
    # number of chromosomes that are aneuploids. 1 aneuploid = n.
  }
  return(sum(embryo@ploidy[isSampled, chromosome] != embryo@euploidy))
})

#' Take a biopsy of the given embryo, return an int corresponding to the biopsy types
#' 2 = aneuploid
#' 1 = mosaic
#' 0 = euploid
#' @param em the embryo for the biopsy sampling
#' @param biop.size the number of cells to be sampled
#' @return an integer representing the biopsy type

take_biopsy <- function(em, biop.size = 5){
  biopsy.result <- takeBiopsy(embryo = em, biopsy.size = biop.size);
  # assign types based on results
  if(biopsy.result == 0){ # no aneuploid cells -- an euploid embryo
    return(0)
  }else if (biopsy.result == biop.size){ # all aneuploid cells -- an aneuploid embryo
    return(2)
  }else{ # mosaic
    return(1)
  }
}
