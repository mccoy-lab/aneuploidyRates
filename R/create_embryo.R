#' This file creates an embryo object.
#' code used from line 9-   are referenced from the Tessera package
#' 

# Set up Embryo -----------------------------------------------------------



#' An S4 class to model an embryo
#'
#' @slot x numeric. x coordinates of cells
#' @slot y numeric.  y coordinates of cells
#' @slot z numeric.  z coordinates of cells
#' @slot aneu numeric. fraction of aneuploid cells
#' @slot disp numeric. fraction of dispersal of cells
#' @slot dists data.frame. pairwise distances between cells
#' @slot neighbours data.frame, whether two cells are near each other
#' @slot euploidy numeric. number of chromosomes considered euploid
#' @slot ploidy data.frame. number of chromosomes per cell
#'
setClass(
  "Embryo",
  # Creates an "Embryo" class
  
  # Define fields or slots that an embryo should have
  representation(
    x = "numeric",
    # numeric types are decimal values
    y = "numeric",
    z = "numeric",
    aneu = "numeric",
    disp = "numeric",
    dists = "data.frame",
    # Displayed like a table. Each column same data type
    neighbours = "data.frame",
    # Why is this not explained?
    euploidy = "numeric",
    ploidy = "data.frame"
  ),
  
  # Define defaults, the values for the embryo when nothing is set
  prototype(
    x = NA_real_,
    y = NA_real_,
    z = NA_real_,
    aneu = NA_real_,
    disp = NA_real_,
    dists = data.frame(),
    # empty table
    neighbours = data.frame(),
    euploidy = NA_real_,
    ploidy = data.frame()
  ),
)

#' Create an embryo
#'
#' A sphere of cells is created with the given proportion of aneuploidies.
#' Aneuploid cells are either adjacent or dispersed. The concordance of aneuploid
#' cells for each chromosome can be specificed; if fully concordant, a cell aneuploid
#' for chr1 will also be aneuploid for chr2 and all the other chrs.
#'
#' @param n.cells the number of cells in the embryo
#' @param n.chrs the number of chromosome pairs per cell
#' @param prop.aneuploid the proportion vector of aneuploid cells (0-1) per chromosome
#' @param dispersal the dispersion vector of the aneuploid cells (0-1)
#' @param concordance the concordance between aneuploid cells for each chromosome (0-1).
#' @param embryo.size.fixed if true, the embryo is exactly the size in \code{n.cells}. If false, the embryo
#' size can vary according to \code{embryo.size.sd}.
#' @param embryo.size.sd the standard deviation of cell number if \code{embryo.size.fixed} is false.
#' The actual embryo size will be sampled from a normal distribution with mean of \code{n.cells} and
#' standard deviation \code{embryo.size.sd}.
#' @param euploidy the number of copies of a chromosome to consider euploid. For a diploid embryo this should be 2.
#' @param rng.seed the seed for the rRandom Number Generation (RNG). Defaults to NULL. Use this to get the same embryo each time
#'
#' @return an Embryo object
#'
#' @examples
#' Create an embryo with 200 cells, 20% aneuploid and a single pair of chromosomes
#' per cell. Aneuploid cells are highly dispersed
# embryo <- Embryo(n.cells = 200, n.chrs = 1,  prop.aneuploid = 0.2,
#                  dispersal =  0.9)
#'
#' Create the embryo above, but using a fixed seed for the random number generator
#' so the resulting embryo is reproducible.
#' embryo <- Embryo(n.cells = 200, n.chrs = 1,  prop.aneuploid = 0.2,
#'                  dispersal =  0.9, rng.seed = 42)
#'
#' Create an embryo with 3 pairs of chromosomes per cell, with all chromosome pairs
#' aneuploid in the same cells.
#' embryo <- Embryo(n.cells = 200, n.chrs = 3,  prop.aneuploid = 0.2,
#'                  dispersal =  0.9, concordance = 1)
#'
#' As above, but specifying a different aneuploidy level for each chromosome pair.
#' embryo <- Embryo(n.cells = 200, n.chrs = 3,  prop.aneuploid = c(0.2, 0.1, 0.4),
#'                  dispersal =  0.9)
Embryo <-
  function(n.cells = 200,
           n.chrs = 1,
           prop.aneuploid = 0.2,
           dispersal = 0.1,
           concordance = 1,
           embryo.size.fixed = T,
           embryo.size.sd = 5,
           euploidy = 2,
           rng.seed = NULL) {
    set.seed(rng.seed)
    
    # All the error messages====================================================
    
    if (embryo.size.sd <= 0) {
      stop(paste0(
        "Number of cells sd (",
        embryo.size.sd,
        ") must be greater than 0"
      ))
    }
    
    if (n.cells <= 1) {
      stop(paste0("Number of cells (", n.cells, ") must be greater than 1"))
    }
    
    if (euploidy <= 0) {
      stop(
        paste0(
          "Number of chromosomes considered euploid (",
          euploidy,
          ") must be greater than 0"
        )
      )
    }
    
    if (n.chrs < 1) {
      stop(paste0(
        "Number of chromosome pairs (",
        n.chrs,
        ") must be greater than 0"
      ))
    }
    
    if (any(prop.aneuploid < 0) | any(prop.aneuploid > 1)) {
      stop(paste0(
        "prop.aneuploid (",
        paste(prop.aneuploid, collapse = ", "),
        ") must be between 0 and 1 inclusive"
      ))
    }
    
    if (any(dispersal < 0) | any(dispersal > 1)) {
      stop(paste0(
        "dispersal (",
        paste(dispersal, collapse = ", "),
        ") must be between 0 and 1 inclusive"
      ))
    }
    
    if (concordance < 0 | concordance > 1) {
      stop(paste0(
        "Concordance (",
        concordance,
        ") must be between 0 and 1 inclusive"
      ))
    }
    
    if (n.chrs > 1 &
        length(prop.aneuploid) == 1)
      prop.aneuploid <- rep(prop.aneuploid, n.chrs)
    if (n.chrs > 1 &
        length(dispersal) == 1)
      dispersal <- rep(dispersal, n.chrs)
    
    if (n.chrs > 1 & length(prop.aneuploid) != n.chrs) {
      stop(
        paste0(
          "Length of prop.aneuploid (",
          length(prop.aneuploid),
          ") must match the number of chromosomes in n.chrs (",
          n.chrs,
          ")"
        )
      )
    }
    
    # Calculate the number of cells to use if not fixed ========================
    if (!embryo.size.fixed) {
      n.cells <- max(1, ceiling(rnorm(1, # use a normal distribution
                                      mean = n.cells,
                                      sd = embryo.size.sd)))
    }
    
    # Set up coordinates (x, y, z), distance, and neighbor data ================
    
    # Make a sphere of evenly spaced points using the Fibonacci lattice
    indices <- seq(0, n.cells - 1, 1) + 0.5
    phi <-
      acos(pmin(pmax(1 - 2 * indices / n.cells,-1.0), 1.0)) # constrain to avoid rounding errors
    theta <- pi * (1 + sqrt(5)) * indices
    
    x <- cos(theta) * sin(phi)
    y <- sin(theta) * sin(phi)
    z <- cos(phi)
    d <- as.data.frame(cbind(x, y, z)) # distance matrix
    n <- as.data.frame(cbind(x, y, z)) # neighbors matrix
    
    # Assign the number of neighbors each cell should have
    .N_NEIGHBOURS <- 6
    # Create distance matrix for each point
    # Set the .N_NEIGHBOURS closest points to be neighbours
    for (i in 1:nrow(d)) {
      dist <-
        sqrt((d$x - x[i]) ** 2 + (d$y - y[i]) ** 2 + (d$z - z[i]) ** 2) # distance between every other points and the current point
      d[[paste0("d", i)]] <-
        dist # create a column to store the distances
      # A point is a neighbour if it is not this point, and it is in the list of closest points
      n[[paste0("n", i)]] <-
        dist > 0 &
        dist <= max(head(sort(dist), n = .N_NEIGHBOURS + 1)) # if the distance is smaller than the 7th on the list
    }
    
    # Set up ploidy data =======================================================
    
    # Make a column for each chromosome. For now everyone is an euploid.
    ploidy <-
      data.frame(matrix(
        data = euploidy,
        nrow = n.cells,
        ncol = n.chrs
      ))
    colnames(ploidy) <- paste0("chr", 1:n.chrs)
    
    #' Set a cell to contain an aneuploid chromosome
    #'
    #' @param ploidy the embryo
    #' @param cell.index the cell to affect
    #' @param chromosome the chromosome to make aneuploid
    #'
    #' @return the modified ploidy table
    set.aneuploid <- function(ploidy, cell.index, chromosome) {
      if (chromosome < 1 | chromosome > n.chrs) {
        stop(paste0("Chromosome must be in range 1-", n.chrs))
      }
      ploidy[cell.index, chromosome] <-
        0 # For now, we just model all aneuploidy as nullisomy
      return(ploidy)
    }
    
    # Test if any of the neighbouring cells have an aneuploidy
    #' @param n the neighbor matrix for cells
    #' @param ploidy the ploidy matrix for cells and chromosomes
    #' @param index the cell to test
    #' @param chromosome the chromosome to test
    #' @param euploidy the number of chromosome to be considered an euploid
    #'
    #' @return Returns true if any of the closest cells are aneuploid
    .has.adjacent.aneuploid <-
      function(n, ploidy, index, chromosome, euploidy) {
        adj.list <- n[[paste0("n", index)]]
        isAenu <-
          ploidy[, chromosome] != euploidy # all rows of chromosome column
        return(any(adj.list & isAenu))
      }
    
    # Test if the given chromosome in the given cell is aneuploid
    #
    #' @param embryo the embryo
    #' @param cell.index the cell to test (0 for all cells)
    #' @param chromosome the chromosome to test
    #' @param euploidy the number of chromosome to be considered an euploid
    #'
    #' @return if cell.index is >0, return true if the chromosome is aneuploid,
    # false otherwise. If cell.index is 0, return true if any chromosome is
    # aneuploid.
    is.aneuploid <-
      function(ploidy,
               cell.index,
               chromosome,
               euploidy) {
        if (chromosome < 1 | chromosome > n.chrs) {
          stop(paste0("Chromosome must be in range 1-", n.chrs))
        }
        
        if (cell.index > n.cells) {
          stop(paste0("Cell index must be in range 0-", n.cells))
        }
        
        # cat("Testing aneuploidy of cell", cell.index, "chr", chromosome, "\n")
        
        # Return a vector if all cells requested
        if (cell.index == 0) {
          return(ploidy[, chromosome] != euploidy)
        }
        
        # Otherwise return just the single cell value
        return(ploidy[cell.index, chromosome] != euploidy)
      }
    
    #' Set aneuploidies in an embryo
    #'
    #' Aneuploid cells are either adjacent or dispersed
    #'
    #' @param ploidy the ploidy matrix from the embryo
    #' @param chromosome the chromosome to set aneuploidies for
    #' @param prop.aneuploid the proportion of aneuploid cells (0-1)
    #' @param dispersion the dispersion of the aneuploid cells (0-1)
    #' @param concordance the concordance between aneuploid cells for each chromosome (0-1).
    #
    #' @return the ploidy matrix with aneuploidies
    set.aneuploidies <-
      function(ploidy,
               chromosome,
               prop.aneuploid,
               dispersion,
               concordance) {
        # Shortcut the easy cases
        if (prop.aneuploid == 0) {
          return(ploidy)
        }
        
        if (prop.aneuploid == 1) {
          for (i in 1:nrow(ploidy)) {
            ploidy <- set.aneuploid(ploidy, i, chromosome)
          }
          return(ploidy)
        }
        
        # set number of aneuploid cells ########################################
        
        # When testing at a small probability, sometimes there is no aneuploid cells
        if (n.cells * prop.aneuploid < 1) {
          # So that for a small prop, sometimes it's 1
          random <- runif(1)
          if (random < prop.aneuploid) {
            n.aneuploid <- 1
          } else{
            n.aneuploid <- 0
          }
        } else{
          n.aneuploid <- round(max(1, n.cells * prop.aneuploid))
        }
        # cat("Creating", n.aneuploid, "aneuploid cells for chr", chromosome,"\n")
        
        # set number of concordant cells #######################################
        
        # Decide how many cells need to be concordant with the previous
        # chromosome (if we are above chromosome 1)
        n.concordant <- 0
        concordant.cells <-
          rep(F, nrow(ploidy)) # default: set all to have no concordant
        if (chromosome > 1) {
          prev.chr <- chromosome - 1 # to be used when chr>1 only
          concordant.cells <-
            is.aneuploid(ploidy, 0, prev.chr, euploidy) # a set of booleans
          # cat("Prev chr", prev.chr, "has", length(concordant.cells[concordant.cells==T]), "aneuploid cells\n")
          n.concordant <-
            length(concordant.cells[concordant.cells == T]) * concordance
          # if there are more aneuploids in the prev chromosome, we can't match all
          n.concordant <- min(n.aneuploid, n.concordant)
          # cat("Expecting", n.concordant, "concordant cells with chr", prev.chr, "\n")
        }
        
        # create the dispersion ################################################
        
        # The approach for dispersal is to set seed cells which will
        # grow into separate aneuploid patches. The more dispersion, the more
        # initial seeds.
        
        # Short cut: skip dispersal if no aneuploidy to be made
        if (n.aneuploid > 0) {
          # Choose number of seeds for aneuploid regions
          n.seeds <- ceiling(max(1, n.aneuploid * dispersion))
          n.to.make <- n.seeds
          
          # Disperse seeds as much as possible initially
          # We create a list of possible seed locations, and as each seed is assigned
          # we remove it and its neighbours
          # If there are enough seeds required, we will exhaust the possible locations
          # and finish seeding in the next step
          # cat("Creating", n.to.make, "seeds\n")
          possible.initial.seeds <-
            1:n.cells # vector of indexes we can sample
          while (n.to.make > 0 &
                 length(possible.initial.seeds) > 0) {
            # Take the next seed from those available. NOTE: if the vector is length 1,
            # sample will sample from 1:n, so we need to correct for this
            seed <-
              ifelse(
                length(possible.initial.seeds) == 1,
                possible.initial.seeds[1],
                sample(possible.initial.seeds, 1)
              )
            if (n.concordant > 0 & !concordant.cells[seed]) {
              possible.initial.seeds <-
                possible.initial.seeds[!possible.initial.seeds %in% c(seed)]
              next # skip non concordant cells
            }
            
            if (.has.adjacent.aneuploid(n, ploidy, seed, chromosome, euploidy)) {
              possible.initial.seeds <-
                possible.initial.seeds[!possible.initial.seeds %in% c(seed)]
              next # spread seeds out
            }
            
            ploidy <-
              set.aneuploid(ploidy, seed, chromosome) # set the seed
            
            # Remove seed and its neighbours from the possible seed list
            possible.initial.seeds <-
              possible.initial.seeds[!possible.initial.seeds %in% c(seed, which(n[[paste0("n", seed)]]))]
            n.to.make <- n.to.make - 1L
            n.concordant <- max(0, n.concordant - 1L)
          }
          
          # When all dispersed seeds have been added, add the remaining seeds randomly
          while (n.to.make > 0) {
            seed <- sample.int(n.cells, 1) # can be simplified?
            if (is.aneuploid(ploidy, seed, chromosome, euploidy))
              next
            if (n.concordant > 0 &
                !concordant.cells[seed])
              next # skip non concordant cells
            ploidy <- set.aneuploid(ploidy, seed, chromosome)
            n.to.make <- n.to.make - 1L
            n.concordant <- max(0, n.concordant - 1L)
          }
          
          # Grow the seeds into neighbouring cells for remaining aneuploid cells
          n.to.make <-
            n.aneuploid - n.seeds # the rest of the aneuploids
          
          # Handle any remaining concordant cells first - the placement rules don't apply
          while (n.to.make > 0) {
            seed <- sample.int(n.cells, 1)
            if (n.concordant > 0) {
              # cat("Placing ", n.concordant, "concordant aneuploid cells\n")
              if (!concordant.cells[seed])
                next
              if (is.aneuploid(ploidy, seed, chromosome, euploidy))
                next # skip cells already aneuploid
              ploidy <- set.aneuploid(ploidy, seed, chromosome)
              n.concordant <- max(0, n.concordant - 1L)
            } else {
              # Grow from an existing seed.
              if (is.aneuploid(ploidy, seed, chromosome, euploidy))
                next # skip cells already aneuploid
              if (!.has.adjacent.aneuploid(n, ploidy, seed, chromosome, euploidy))
                next # only grow next to existing aneuploid
              ploidy <- set.aneuploid(ploidy, seed, chromosome)
            }
            
            n.to.make <- n.to.make - 1
          }
        }
        return(ploidy)
      }
    
    
    # Set the aneuploid cells in the ploidy matrix
    for (chr in 1:n.chrs) {
      ploidy <- set.aneuploidies(ploidy,
                                 chr,
                                 prop.aneuploid[chr],
                                 dispersal[chr],
                                 concordance)
    }
    
    
    new(
      "Embryo",
      x = d[, 1],
      y = d[, 2],
      z = d[, 3],
      aneu = prop.aneuploid,
      disp = dispersal,
      dists = d[,-(1:3)],
      neighbours = n[,-(1:3)],
      euploidy = euploidy,
      ploidy = ploidy
    )
    
    
  }

# Override functions --------------------------------------------------------

# Add a method override for existing generic function length to get the number of cells
setMethod("length", "Embryo", function(x) {
  length(x@x)
})


# Override show function for an Embryo object
setMethod("show", "Embryo", function(object) {
  cat(
    "Embryo with ",
    length(object@x),
    " cells\n",
    ncol(object@ploidy),
    " chromosome pairs per cell\n",
    object@euploidy,
    " copies of each euploid chromosome per cell\n",
    "Aneuploidy: ",
    object@aneu,
    " dispersal ",
    object@disp,
    "\n",
    sep = ""
  )
})