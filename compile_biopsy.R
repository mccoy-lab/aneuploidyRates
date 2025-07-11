# This file combines results from all parallel jobs and evaluates the percentage 
# of aneuploid biopsies that are from mosaic embryos.
# Namely, the proportion of discarded embryos that could be viable for transfer.
library(readr)
library(dplyr)

# Compile results
results <- list.files("2025-07-10_results", full.names = TRUE, pattern = "*.csv")
combined <- bind_rows(lapply(results, read_csv))

final_summary <- combined %>%
  group_by(dispersal, embryo_type, first_biopsy_type, second_biopsy_type) %>%
  summarise(total = sum(count), .groups = "drop") %>%
  arrange(desc(total))

write_csv(final_summary, "final_summary.csv")