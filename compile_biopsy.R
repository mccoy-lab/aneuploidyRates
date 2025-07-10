library(readr)
library(dplyr)

results <- list.files("2025-07-09_results", full.names = TRUE, pattern = "*.csv")
combined <- bind_rows(lapply(results, read_csv))

final_summary <- combined %>%
  group_by(dispersal, biopsy_type, embryo_type, biopsy_cell) %>%
  summarise(total = sum(count), .groups = "drop") %>%
  arrange(desc(total))

write_csv(final_summary, "final_summary.csv")
