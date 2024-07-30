#' Create an embryo with the given proportion of aneuploidy cells.
#'
#' @param n.cells the number of cells in the embryo
#' @param n.chrs the number of chromosome pairs per cell
#' @param prop.aneuploid the proportion vector of aneuploid cells (0-1) per chromosome
#' @param dispersal the dispersion vector of the aneuploid cells (0-1)
#' @param concordance the concordance between aneuploid cells for each chromosome (0-1).
#' @param euploidy the number of copies of a chromosome to consider euploid.
#' For a diploid embryo this should be 2.
#' @param rng.seed the seed for the Random Number Generation (RNG). Defaults to NULL.
#' Use this to obtain the same embryo each time
#'
#' @export
#' @return an Embryo object
#'
#' @examples
#' embryo <- create_embryo(n.cells = 200, n.chrs = 1,  prop.aneuploid = 0.2,
#' dispersal =  0.9)
#'
#' embryo <- create_embryo(n.cells = 200, n.chrs = 1,  prop.aneuploid = 0.2,
#' dispersal =  0.9, rng.seed = 42)
#'
#' embryo <- create_embryo(n.cells = 200, n.chrs = 3,  prop.aneuploid = 0.2,
#' dispersal =  0.9, concordance = 1)
#'
#' embryo <- create_embryo(n.cells = 250, n.chrs = 3,
#' prop.aneuploid = c(0.2, 0.1, 0.4), dispersal =  0.9)
create_embryo <-
  function(n.cells = 200,
           n.chrs = 1,
           prop.aneuploid = 0.2,
           dispersal = 0,
           concordance = 0,
           euploidy = 2,
           rng.seed = NULL) {
    return(
      tessera::Embryo(
        n.cells = n.cells,
        n.chrs = n.chrs,
        prop.aneuploid = prop.aneuploid,
        dispersal = dispersal,
        concordance = concordance,
        euploidy = euploidy,
        rng.seed = rng.seed
      )
    )

  }

