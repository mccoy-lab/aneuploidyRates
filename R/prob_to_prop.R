#' This file converts the probabilities of being affected
#' by meiotic and mitotic errors to proportions of
#' aneuploid cells within an embryo.


#' Convert the probabilities to proportion.
#' @param prob_meio the probability of having a meiotic error
#' @param prob_mito the probability of having a mitotic error
#' @param num_division the total number of divisions in this embryo
#'
#' @return proportion of totally affected (aneuploidy) cells within
#' this embryo.

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
#' Recursive.
#' The derived cells of an aneuploid cell will all be aneuploids. Therefore,
#' we can tally the number of affected cells mathematically.
#'
#' @param cell.affected current total number of affected cells
#' @param n.division number of divisions simulated
#' @param prob.affected the likelihood of a cell to be an aneuploid.
#' @param currently.affected True if the current cell is an aneuploid and False otherwise
#'
#' @return total number of affected cells in this simulation
#'
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
  
  # Short cut: since every cell derived from an affected cell
  # is also affected, we can just calculate and return the tally.
  if (currently.affected) {
    # add all its children cells to cells affected
    cells.affected <- cells.affected + (2 ^ n.division)
    return(cells.affected)
  }
  
  # if the cells are still dividing
  if (n.division != 0) {
    # for the next two cells it splits into
    for (i in 1:2) {
      rand <- runif(1)
      # randomly set if the next cell is affected
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
