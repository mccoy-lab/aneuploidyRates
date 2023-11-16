#' This file loads the generated data of meiotic and mitotic rates and plots them
#' using different visualization methods.
library(ggplot2)

# -------------Load and Process Data-------------------------
my_data <- read.table("data/1.txt",header = TRUE, sep = "\t")
my_data2 <- read.table("data/2.txt",header = TRUE, sep = "\t")
ggplot(data = my_data) + geom_histogram()
