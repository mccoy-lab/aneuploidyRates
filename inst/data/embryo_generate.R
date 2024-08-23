library(dplyr)

#### Misdiagnosed Rates ##############
date <- "2024-08-21"
data <- c()
args <- commandArgs(trailingOnly = TRUE)
id <- strtoi(args[1]) - 1

prob_to_prop <- function(prob.meio, prob.mito, num.division = 8) {
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

mito_aneu_cells <- function(cells.affected = 0,
                            n.division = 8,
                            prob.affected = 0.5,
                            currently.affected = F) {
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

# For each misclassification error level
embryos <- c()
  # Extract the posterior distribution
  data <- read.csv(paste0(args[2],"/", date, "_", id, "/data.csv"))
  data <- cbind(data, misdiagnosed.rates = id * (1/10))
  posterior <- data[colnames(data) %in% c("prob.meio", "prob.mito")]
  
  
  # Draw a sample from the posterior distribution
  pair <- posterior[sample(nrow(posterior), size = 1000, replace = TRUE, prob = data$weights),]

  for(i in 1:1000) {
    pair_proportion <- matrix(0, ncol = 3)
    colnames(pair_proportion) <- c("Euploid", "Mosaic Aneuploid", "Fully Aneuploid")
    # Generate 1000 embryos and report number of euploid, mosaic aneuploid, and fully aneuploid
    for(j in 1:1000) {
      prop <- prob_to_prop(prob.meio = pair[i,1], prob.mito = pair[i,2])
      if (prop == 0) {
        pair_proportion[[1]] = pair_proportion[[1]] + 1
      } else if (prop == 1) {
        pair_proportion[[3]] = pair_proportion[[3]] + 1
      } else {
        pair_proportion[[2]] = pair_proportion[[2]] + 1
      }
    }
    pair_proportion <- cbind(pair[i,], pair_proportion, misclassification = id * (1/10))
    embryos <- rbind(embryos, pair_proportion)
  }


write.csv(embryos, args[3])

