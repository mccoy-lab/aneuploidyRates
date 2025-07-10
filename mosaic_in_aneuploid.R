# This file evaluates the percentage of aneuploid biopsies that are from mosaic embryos.
# Namely, the proportion of discarded embryos that could be viable for transfer.
library(dplyr)

# Obtain saved embryo data
data1 <- read.csv("data/2025-04-04c/full_data.csv")
data2 <- read.csv("data/2025-04-04d/full_data.csv")
data3 <- read.csv("data/2025-04-04e/full_data.csv")
# biopsy_data <- rbind(data1, data2, data3)
# biopsy_data <- data1

# Combine and slice into chunks
all_data <- bind_rows(data1, data2, data3)

chunk_size <- 5000
n_chunks <- ceiling(nrow(all_data) / chunk_size)

results_list <- list()

for (i in seq_len(n_chunks)) {
  cat("Processing chunk", i, "of", n_chunks, "\n")
  
  data_chunk <- all_data %>%
    slice(((i - 1) * chunk_size + 1):(min(i * chunk_size, n())))
  
  processed_chunk <- data_chunk %>%
    rowwise() %>%
    mutate(
      prop.aneuploid = runif(1, 0.2, 0.6),
      embryo = list(tessera::Embryo(
        n.cells = 256,
        prop.aneuploid = prop.aneuploid,
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
  
  results_list[[i]] <- processed_chunk
}

# # take a biopsy for every single embryo
# biopsy_data <- biopsy_data %>%
#   rowwise() %>%
#   mutate(
#     embryo = list(tessera::Embryo(
#       n.cells = 256,
#       prop.aneuploid = prop.aneu,
#       dispersal = dispersal,
#       rng.seed = NULL
#     )),
#     biopsy_cell = tessera::takeBiopsy(embryo, biopsy.size = 5)
#   )%>% 
#   ungroup() %>%# categorize the biopsy
#   mutate(
#     biopsy_type = case_when(
#       biopsy_cell < 5 * 0.3 ~ "Euploid",
#       biopsy_cell > 5 * 0.3 & biopsy_cell < 5 * 0.7 ~ "Mosaic",
#       biopsy_cell > 5 * 0.7 ~ "Aneuploid"
#     )
#   ) %>%  # categorize the embryo
#   mutate(
#     embryo_type = case_when(
#       prop.aneu == 0 ~ "Euploid",
#       prop.aneu > 0 & prop.aneu < 1 ~ "Mosaic Aneuploid",
#       prop.aneu == 1 ~ "Fully Aneuploid"
#     )
#   ) %>%
#   group_by(dispersal, biopsy_type, embryo_type, biopsy_cell) %>%
#   summarise(count = n(), .groups = "drop") %>%
#   arrange(desc(count))
# 
# print(biopsy_data)
# 
# test <- data1 %>%
#   arrange(prop.aneu)
# print(head(test))

# # For loop
# biopsy_data <- data1
# # Initialize empty vectors to store results
# embryo_list <- list()
# biopsy_cell_vec <- numeric(nrow(biopsy_data))
# biopsy_type_vec <- character(nrow(biopsy_data))
# embryo_type_vec <- character(nrow(biopsy_data))
# 
# 
# # Loop through each row of the dataset
# for (i in seq_len(nrow(biopsy_data))) {
#   prop_aneu <- 0.2
#   dispersal_val <- as.numeric(biopsy_data$dispersal[i])
#   
#   # print(paste0(prop_aneu, " + ", dispersal_val))
#   # Create embryo object
#   embryo <- tessera::Embryo(
#     n.cells = 256,
#     prop.aneuploid = prop_aneu,
#     dispersal = dispersal_val,
#     rng.seed = NULL
#   )
#   
#   # Save embryo object if needed
#   embryo_list[[i]] <- embryo
#   
#   # Take biopsy of 5 cells
#   biopsy_result <- tessera::takeBiopsy(embryo, biopsy.size = 5)
#   print(paste0(prop_aneu, " + ", dispersal_val, " : ", biopsy_result))
#   biopsy_cell_vec[i] <- biopsy_result
#   
#   # Categorize biopsy
#   if (biopsy_result < 5 * 0.3) {
#     biopsy_type_vec[i] <- "Euploid"
#   } else if (biopsy_result < 5 * 0.7) {
#     biopsy_type_vec[i] <- "Mosaic"
#   } else {
#     biopsy_type_vec[i] <- "Aneuploid"
#   }
#   
#   # Categorize embryo
#   if (prop_aneu == 0) {
#     embryo_type_vec[i] <- "Euploid"
#   } else if (prop_aneu == 1) {
#     embryo_type_vec[i] <- "Fully Aneuploid"
#   } else {
#     embryo_type_vec[i] <- "Mosaic Aneuploid"
#   }
# }
# 
# # Combine results into a new dataframe
# biopsy_results <- data.frame(
#   dispersal = biopsy_data$dispersal,
#   biopsy_cell = biopsy_cell_vec,
#   biopsy_type = biopsy_type_vec,
#   embryo_type = embryo_type_vec
# )
# 
# write.csv(biopsy_results,"biopsy_results.csv", row.names = FALSE)
# 
# # Summarize
# summary_table <- aggregate(
#   x = list(count = rep(1, nrow(biopsy_results))),
#   by = list(
#     dispersal = biopsy_results$dispersal,
#     biopsy_type = biopsy_results$biopsy_type,
#     embryo_type = biopsy_results$embryo_type,
#     biopsy_cell = biopsy_results$biopsy_cell
#   ),
#   FUN = length
# )
# 
# # Sort by count descending
# summary_table <- summary_table[order(-summary_table$count), ]
# 
# # Output
# print(summary_table)
