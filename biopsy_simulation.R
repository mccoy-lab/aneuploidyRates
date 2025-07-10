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

# Combine and find the corresponding rows
all_data <- bind_rows(data1, data2, data3)

current_data <- all_data %>%
  slice(start_row:end_row)

# Process this group of data
cat("Processing group ", task_id, "\n")

processed_data <- current_data %>%
  rowwise() %>%
  mutate(
    embryo = list(tessera::Embryo(
      n.cells = 256,
      prop.aneuploid = prop.aneu,
      dispersal = dispersal,
      rng.seed = NULL
    )),
    biopsy_cell = tessera::takeBiopsy(embryo, biopsy.size = 5)
  ) %>%
  ungroup() %>%
  mutate(
    biopsy_type = case_when(
      biopsy_cell < 5 * 0.3 ~ "Euploid",
      biopsy_cell >= 5 * 0.3 & biopsy_cell <= 5 * 0.7 ~ "Mosaic",
      biopsy_cell > 5 * 0.7 ~ "Aneuploid"
    ),
    embryo_type = case_when(
      prop.aneuploid == 0 ~ "Euploid",
      prop.aneuploid > 0 & prop.aneuploid < 1 ~ "Mosaic Aneuploid",
      prop.aneuploid == 1 ~ "Fully Aneuploid"
    )
  ) %>%
  group_by(dispersal, biopsy_type, embryo_type, biopsy_cell) %>%
  summarise(count = n(), .groups = "drop")

# Write output to file
out_path <- file.path(outdir, paste0("group_", task_id, ".csv"))
write_csv(processed_data, out_path)