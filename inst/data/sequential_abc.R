# This file runs the aneuploidyRates package and writes the generated data to a csv file
#
# source("R/find_rates.R")
# if(!require(aneuploidyRates)){
#   if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org")
#   library(devtools)
#   install_github("mccoy-lab/aneuploidyRates")
# }
# library(aneuploidyRates)

if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
library(dplyr)

if(!require(EasyABC)) install.packages("EasyABC", repos = "http://cran.us.r-project.org")
library(EasyABC)


# Set the model for biopsy summary
rates_model <- function(probs) {

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

  summarize_biopsy <- function(num.em = 1000,
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
      # em <- create_embryo(
      #   prop.aneuploid = prop.aneu,
      #   n.cells = num.cell,
      #   n.chrs = num.chr,
      #   dispersal = dispersal,
      #   concordance = concordance
      # )

      em <- tessera::Embryo(
        n.cells = num.cell,
        n.chrs = num.chr,
        prop.aneuploid = prop.aneu,
        dispersal = dispersal,
        concordance = concordance,
        euploidy = 2,
        rng.seed = NULL
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


  set.seed(probs[[1]])

  biopsy <- summarize_biopsy(
    meio = probs[[2]],
    mito = probs[[3]],
    dispersal = probs[[4]],
    num.cell = 256,
    hide.default.param = TRUE
  )


  # Saves all data (used for displaying prop.aneu and other default params later)
  # remaining.data <<- rbind(remaining.data, biopsy)
  write.csv(biopsy,paste0("temp/", round(probs[[2]],3), "_", round(probs[[3]],3), ".csv"))
  # Returns only the biopsy types
  return(biopsy[1,5:7])
}


  meio.range = list(0, 1)
  mito.range = list(0, 1)
  disp.range = list(0, 0)
  expected = c(0.4287, 0.3532, 0.2181)
  num.trials = 2000
  hide.param = TRUE

  # Set up temp folder
  dir.create("temp/")

  # Choose the distribution to draw inputs. Assume uniform distributions.
  rates_prior <- list(
    c("unif", meio.range[[1]], meio.range[[2]]),
    c("unif", mito.range[[1]], mito.range[[2]]),
    c("unif", disp.range[[1]], disp.range[[2]])
  )

  rates_sim <-
    EasyABC::ABC_sequential(
      method = "Lenormand",
      # previously set biopsy model, returns a list of biopsy type percentages
      model = rates_model,
      # previously set distributions for error rates and dispersal inputs
      prior = rates_prior,
      # number of simulations
      nb_simul = num.trials,
      # expected values, used for selecting results
      summary_stat_target = expected,
      # proportion of particles kept at each step, default to 0.5
      alpha = 0.5,
      # stopping criterion, propotion of new particles accepted, default 0.05
      p_acc_min = 0.05,
      use_seed = TRUE,
      n_cluster = 44,
      progress_bar = TRUE
    )

  print(rates_sim)

  # Set up return format: from the saved data, select the rows with ABC_rej's
  # returned parameters
  result <- cbind(rates_sim$param[,1:2], 0, rates_sim$stats)

  # keeping the weights
  result<- cbind(result, rates_sim$weights)


  # collect prop.aneu
  result_prop_aneu <- c()
  for(i in 1:nrow(result)){
    filename <- paste0("temp/", round(result[i,1], 3), "_", round(result[i,2],3), ".csv")
    proportion <- read.csv(filename)
    proportion <- proportion[,2:8]
    result_prop_aneu <- rbind(result_prop_aneu, proportion)
  }



    colnames(result) <-
      c(
        "prob.meio",
        "prob.mito",
        "dispersal",
        "euploid",
        "mosaic",
        "aneuploid",
        "weights"
      )

    colnames(result_prop_aneu) <-
      c(
        "prop.aneu",
        "prob.meio",
        "prob.mito",
        "dispersal",
        "euploid",
        "mosaic",
        "aneuploid"
      )


  # Write to file
  args <- commandArgs(trailingOnly = TRUE)
  write.csv(result_prop_aneu, file = args[1])
  write.csv(result, file = args[2])

  unlink("temp/*", recursive = TRUE)
