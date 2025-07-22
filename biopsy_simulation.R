# This file creates embryos based on the stored proportion of aneuploidy and dispersal,
# and takes two biopsies for each embryo. It outputs a summary file filtered by the 
# biopsy and embryo type

library(dplyr)
library(readr)
library(stringr)

# Load arguments
args <- commandArgs(trailingOnly = TRUE)
start_row <- as.integer(args[1])
end_row <- as.integer(args[2])
task_id <- as.integer(args[3])
outdir <- args[4]

# Obtain saved embryo data
data1 <- read.csv("data/2025-04-04c/full_data.csv")
data2 <- read.csv("data/2025-04-04d/full_data.csv")
data3 <- read.csv("data/2025-04-04e/full_data.csv")

# Combine and find the corresponding rows for the group
all_data <- bind_rows(data1, data2, data3)

current_data <- all_data %>%
  slice(start_row:end_row)

# Process this group of data
cat("Processing group ", task_id, "\n") # signal to start

# Simulate re-biopsy
processed_data <- current_data %>%
  rowwise() %>% # row-by-row operation
  mutate(
    embryo = list(tryCatch({ # error catching to avoid restarting the whole procedure
      tessera::Embryo( # create an embryo
        n.cells = 256,
        n.chr = 1,
        prop.aneuploid = prop.aneu,
        dispersal = dispersal,
        concordance = 0,
        euploidy = 2,
        rng.seed = NULL
      )
    }, error = function(e) {
      warning(paste("Embryo creation failed on task", task_id, "row with prop.aneu =", prop.aneu))
      NULL
    })), # first biopsy: starting from default index cell
    first_biopsy_cell = tessera::takeBiopsy(embryo, biopsy.size = 5), 
    # second biopsy: starting form a random cell
    second_biopsy_cell = tessera::takeBiopsy(embryo, biopsy.size = 5, index.cell = sample(1:256, 1)),
  ) %>%
  ungroup() %>% # Categorize
  mutate(
    first_biopsy_type = case_when(
      first_biopsy_cell < 5 * 0.2 ~ "Euploid",
      first_biopsy_cell >= 5 * 0.2 & first_biopsy_cell <= 5 * 0.7 ~ "Mosaic",
      first_biopsy_cell > 5 * 0.7 ~ "Aneuploid"
    ),
    second_biopsy_type = case_when(
      second_biopsy_cell < 5 * 0.2 ~ "Euploid",
      second_biopsy_cell >= 5 * 0.2 & second_biopsy_cell <= 5 * 0.7 ~ "Mosaic",
      second_biopsy_cell > 5 * 0.7 ~ "Aneuploid"
    ),
    embryo_type = case_when(
      prop.aneu == 0 ~ "Euploid",
      prop.aneu > 0 & prop.aneu < 1 ~ "Mosaic Aneuploid",
      prop.aneu == 1 ~ "Fully Aneuploid"
    )
  ) 

# Output to file
out_path <- file.path(outdir, paste0("group_", task_id, ".csv"))
write_csv(processed_data, out_path)