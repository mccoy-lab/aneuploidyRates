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
  cells_affected <- 0
  total_cells <- 2 ^ num_division
  
  # Meiotic check:
  has_meio_error <- runif(1) < prob_meio
  if (has_meio_error) {
    return(1)
  } else{
    # Mitotic errors
    return(
      mito_aneu_cells(n.division = num.division,
                      prob.affected = prob.mito) / total_cells
    )
  }
}



#' Simulate cell division and count the number of affected (aneuploid) cells.
#' Recursive.
#' Don't keep track of aneuploidies: just stop and do math.
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
  # print(paste0("CURRENT DIVISION: ", n.division))
  
  if (currently.affected) {
    # stop and add all its children cells to cells affected
    cells.affected <- cells.affected + (2 ^ n.division)
    # print(paste0("cell affected at division: ", n.division))
    # print(paste0("total affected cells: ", cells.affected))
    return(cells.affected)
  }
  
  # if the cells are still dividing
  if (n.division != 0) {
    # for the next two cells it splits into
    for (i in 1:2) {
      rand <- runif(1)
      # print(rand)
      # randomly set if the next cell is affected
      if (rand < prob.affected) {
        cells.affected <-  aneu.cells(
          cells.affected = cells.affected,
          n.division = n.division - 1,
          prob.affected = prob.affected,
          currently.affected = T
        )
      } else{
        cells.affected <-  aneu.cells(
          cells.affected = cells.affected,
          n.division = n.division - 1,
          prob.affected = prob.affected,
          currently.affected = currently.affected
        )
      }
    }
  }
  # if all divisions are completed
  # print(paste0("total cells affected: ", cells.affected))
  return(cells.affected)
}