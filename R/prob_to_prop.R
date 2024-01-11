#' Convert the error rates to proportion of aneuploidy cells within an embryo.
#'
#' Affect the cell with meiotic aneuploidy based on error rate. If the cell is
#' affected, return the proportion of aneuploidy as 1. Else, simulate cell division
#' with random mitotic errors based on the probability and calculate the number of
#' aneuploid cells by the end of the simulation. Return the proportion.
#'
#' @param prob.meio the probability of having a meiotic error
#' @param prob.mito the probability of having a mitotic error
#' @param num.division the total number of divisions in this embryo
#'
#' @return proportion of totally affected (aneuploidy) cells within
#' this embryo.
#' @export
#'
#' @examples
#' prob_to_prop(0, 0.5)
#' prob_to_prop(0.2, 0.03, num.division = 6)
prob_to_prop <- function(prob.meio, prob.mito, num.division = 8) {
  # Error messages
  if (prob.meio < 0 | prob.mito < 0) {
    stop(paste0(
      "The probabilities: ",
      prob.meio,
      ", ",
      prob.mito,
      " must be at least 0"
    ))
  }
  if (prob.meio > 1 | prob.mito > 1) {
    stop(paste0(
      "The probabilities: ",
      prob.meio,
      ", ",
      prob.mito,
      " must be at most 1"
    ))
  }
  if (num.division %% 1 != 0) {
    stop(paste0(
      "The number of cell division: ",
      num.division,
      "should be an integer"
    ))
  }

  cells.affected <- 0
  total.cells <- 2 ^ num.division

  # Meiotic check:
  has.meio.error <- runif(1) < prob.meio
  if (has.meio.error) {
    # Meiotic-errored embryos always have fully aneuploidy.
    # Thus, prop.aneu = 1
    return(1)
  } else{
    # Mitotic errors
    return(
      mito_aneu_cells(n.division = num.division,
                      prob.affected = prob.mito) / total.cells
    )
  }
}



#' Simulate cell division and count the number of affected (aneuploid) cells.
#'
#' Since the derived cells of an aneuploid cell will all be aneuploids, the
#' function recursively tallies the number of affected cells after a certain
#' number of cell divisions.
#'
#' @param cells.affected current total number of affected cells
#' @param n.division number of divisions simulated
#' @param prob.affected the likelihood of a cell to be an aneuploid.
#' @param currently.affected True if the current cell is an aneuploid and False otherwise
#'
#' @return total number of affected cells in this simulation
mito_aneu_cells <- function(cells.affected = 0,
                            n.division = 8,
                            prob.affected = 0.5,
                            currently.affected = F) {
  # Error messages
  if (prob.affected < 0) {
    stop(paste0(
      "The probability of being affected: ",
      prob.affected,
      " must be at least 0"
    ))
  }
  if (prob.affected > 1) {
    stop(paste0(
      "The probability of being affected: ",
      prob.affected,
      " must be at most 1"
    ))
  }
  if (cells.affected %% 1 != 0) {
    stop(paste0(
      "The number of cells affected: ",
      cells.affected,
      "should be an integer"
    ))
  }
  if (n.division %% 1 != 0) {
    stop(paste0(
      "The number of cell division: ",
      n.division,
      "should be an integer"
    ))
  }

  # The current cell is already affected:
  # Since every cell derived from an affected cell is also affected, we can just
  # calculate the number of cells below this branch and return the tally.
  if (currently.affected) {
    # add all its children cells to cells affected
    cells.affected <- cells.affected + (2 ^ n.division)
    return(cells.affected)
  }


  # The current cell is euploid
  # If the cell is still dividing
  if (n.division != 0) {
    # For the next two cells it splits into, randomly set if the next cell is affected
    # Call this function again to keep dividing/check the derived cells
    for (i in 1:2) {
      rand <- runif(1)
      if (rand < prob.affected) {
        cells.affected <-  mito_aneu_cells(
          cells.affected = cells.affected,
          n.division = n.division - 1,
          prob.affected = prob.affected,
          currently.affected = T
        )
      } else{
        cells.affected <-  mito_aneu_cells(
          cells.affected = cells.affected,
          n.division = n.division - 1,
          prob.affected = prob.affected,
          currently.affected = currently.affected
        )
      }
    }
  }
  # if all divisions are completed
  return(cells.affected)
}
