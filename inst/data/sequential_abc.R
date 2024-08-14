# This file runs the aneuploidyRates package and writes the generated data to a csv file
#
# source("R/find_rates.R")
# if(!require(aneuploidyRates)){
#   if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org")
#   library(devtools)
#   install_github("mccoy-lab/aneuploidyRates")
# }
# library(aneuploidyRates)


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

#
# find_rates <- function(meio.range = list(0, 1),
#                        mito.range = list(0, 1),
#                        disp.range = list(0, 1),
#                        expected = c(0.388, 0.186, 0.426),
#                        tolerance = 0.05,
#                        num.trials = 100,
#                        hide.param = TRUE) {
#   # Error messages
#   if (length(meio.range) != 2 |
#       length(mito.range) != 2 | length(disp.range) != 2) {
#     stop(paste0(
#       "Must input a range (e.g. (0,1)) for the error and dispersal ranges"
#     ))
#   }
#   if (meio.range[1] < 0 | mito.range[1] < 0 | disp.range[1] < 0) {
#     stop(
#       paste0(
#         "The probabilities: ",
#         meio.range[[1]],
#         ", ",
#         mito.range[[1]],
#         ", and dispersal: ",
#         disp.range[[1]],
#         " must be at least 0"
#       )
#     )
#   }
#   if (meio.range[2] > 1 | mito.range[2] > 1 | disp.range[2] > 1) {
#     stop(
#       paste0(
#         "The probabilities: ",
#         meio.range[[2]],
#         ", ",
#         mito.range[[2]],
#         ", and dispersal: ",
#         disp.range[[2]],
#         " must be at most 1"
#       )
#     )
#   }
#   if (num.trials %% 1 != 0) {
#     stop(
#       paste0(
#         "The number of trials for the simulation: ",
#         num.trials,
#         "should be an integer"
#       )
#     )
#   }
#   if (tolerance <= 0 | tolerance > 1) {
#     stop(paste0("The tolerance: ",
#                 tolerance,
#                 " should be in the range (0, 1]"))
#   }
#   if (sum(expected) != 1) {
#     stop(paste0(
#       "The expected percentages of all three embryo types should
#                 sum up to 1"
#     ))
#   }
#   if(num.trials*tolerance <= 1.5){
#     stop(paste0(
#       "The number of selected parameters allowed should be more than 1"
#     ))
#   }

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
  write.csv(biopsy,paste0(probs[[2]], "_", probs[[3]], ".csv"))
  # Returns only the biopsy types
  return(biopsy[1,5:7])
}


meio.range = list(0, 1)
mito.range = list(0, 1)
disp.range = list(0, 0)
expected = c(0.388, 0.186, 0.426)
num.trials = 30
hide.param = TRUE

  # Set up matrix for later output
  remaining.data <- matrix(ncol = 7)
  if (!hide.param) {
    remaining.data <- matrix(ncol = 10)
  }


  print(remaining.data)

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
      p_acc_min = 0.99,
      use_seed = TRUE,
      n_cluster = 5,
      progress_bar = TRUE
    )

  print(rates_sim)
  print(remaining.data)

  # Set up return format: from the saved data, select the rows with ABC_rej's
  # returned parameters
  # result <-
  #   remaining.data[remaining.data[, 2] %in% rates_sim$param[, 1]
  #                  &
  #                    remaining.data[, 3] %in% rates_sim$param[, 2], ]
  # print(result)
  result <- cbind(rates_sim$param[,1:2], 0,rates_sim$stats)
  print(result)

  # Set row names (numbers)
  # rownames(result) <- 1:nrow(result)

  # Set column names
  # if (hide.param) {
  #   colnames(result) <-
  #     c(
  #       "prop.aneu",
  #       "prob.meio",
  #       "prob.mito",
  #       "dispersal",
  #       "euploid",
  #       "mosaic",
  #       "aneuploid"
  #     )
  # } else{
  #   colnames(result) <-
  #     c(
  #       "prop.aneu",
  #       "prob.meio",
  #       "prob.mito",
  #       "dispersal",
  #       "euploid",
  #       "mosaic",
  #       "aneuploid",
  #       "num.cell",
  #       "num.chr",
  #       "concordance"
  #     )
  # }

    colnames(result) <-
      c(
        "prob.meio",
        "prob.mito",
        "dispersal",
        "euploid",
        "mosaic",
        "aneuploid"
      )

  print(data.frame(result))





# find_rates(num.trials = 30, expected = c(0.232, 0.187, 0.581), disp.range = list(0,0), tolerance = 0.8)
# find_rates(num.trials = 30, tolerance = 1)

# Performance check
# system.time(
#   df <- find_rates(num.trials = 50)
# )
# print(typeof(df))
# print(df)
