#' This file loads the generated data of meiotic and mitotic rates and plots them
#' using different visualization methods.
library(ggplot2)
library(readr)
# -------------Load and Process Data-------------------------
# Set column names
my_data_cols <-
  read_table("data/1.txt", n_max  = 1, col_names = FALSE)
my_data_cols <- cbind('embryo', my_data_cols)
my_data <- read_table("data/1.txt", skip = 1, col_names = FALSE)
# my_data <- my_data[,3:ncol(my_data_cols)]
# read headers first
# my_data <- read_table(file = "data/1.txt")
# my_data2 <- read_table("data/2.txt")
print(my_data_cols)
print(my_data[1, ])
print(my_data[, 3])

colnames(my_data) <- my_data_cols[1, ]

# Read all the rest of the data
for (i in 2:100) {
  temp <-
    read_table(paste0("data/", i, ".txt"),
               skip = 1,
               col_names = FALSE)
  colnames(temp) <- my_data_cols[1, ]
  my_data <- rbind(my_data, temp)
}


# print(my_data)

ggplot(data = my_data, aes(x = my_data$prob.meio, y = my_data$prob.mito)) + geom_point()

# 0.01 tolerance
# 95% Quantiles

# Prob. meio
# 2.5%     97.5%
# 0.1583445 0.5501766

# Prob. mito
# 2.5%       97.5%
# 0.009530512 0.126553831
