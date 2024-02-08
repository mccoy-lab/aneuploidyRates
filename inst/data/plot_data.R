# This file loads the generated data of meiotic and mitotic rates and plots them
# using different visualization methods.
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
library(ggplot2)
# if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
library(readr)

# -------------Load and Process Data-------------------------
# Locate the folder to investigate
date <- "2024-01-04"

# Set column names
my_data_cols <-
  read_table(paste0("inst/data/", date, "/1.txt"),
             n_max  = 1,
             col_names = FALSE)
my_data_cols <- cbind('embryo', my_data_cols)

# Read the first txt file
my_data <-
  read_table(paste0("inst/data/", date, "/1.txt"),
             skip = 1,
             col_names = FALSE)
colnames(my_data) <- my_data_cols[1,]

# Read all the rest of the data
for (i in 2:100) {
  temp <-
    read_table(paste0("inst/data/", date, "/", i, ".txt"),
               skip = 1,
               col_names = FALSE)
  colnames(temp) <- my_data_cols[1,]
  my_data <- rbind(my_data, temp)
}


# Draw scatterplots
ggplot(data = my_data, aes(x = my_data$prob.meio, y = my_data$prob.mito)) + geom_point()
ggplot(data = my_data,
       aes(
         x = my_data$prob.meio,
         y = my_data$prob.mito,
         color = my_data$dispersal
       )) + geom_point()


# Combine two tables
# Locate the folder to investigate
date <- "2024-01-12"

# Set column names
new_data_cols <-
  read_table(paste0("inst/data/", date, "/1.txt"),
             n_max  = 1,
             col_names = FALSE)
new_data_cols <- cbind('embryo', new_data_cols)

# Read the first txt file
new_data <-
  read_table(paste0("inst/data/", date, "/1.txt"),
             skip = 1,
             col_names = FALSE)
colnames(new_data) <- new_data_cols[1,]

# Read all the rest of the data
for (i in 2:100) {
  temp <-
    read_table(paste0("inst/data/", date, "/", i, ".txt"),
               skip = 1,
               col_names = FALSE)
  colnames(temp) <- new_data_cols[1,]
  new_data <- rbind(new_data, temp)
}


# Draw scatterplots
ggplot(data = new_data, aes(x = new_data$prob.meio, y = new_data$prob.mito)) + geom_point()
ggplot(data = new_data,
       aes(
         x = new_data$prob.meio,
         y = new_data$prob.mito,
         color = new_data$dispersal
       )) + geom_point()


result <- rbind(new_data,my_data)
# Draw scatterplots
ggplot(data = result, aes(x = result$prob.meio, y = result$prob.mito)) + geom_point()
ggplot(data = result,
       aes(
         x = result$prob.meio,
         y = result$prob.mito,
         color = result$dispersal
       )) + geom_point()


# summary for 01-04 and 01-12 combined:
# embryo       prop.aneu        prob.meio         prob.mito           dispersal            euploid           mosaic
# Min.   : 1.0   Min.   :0.2271   Min.   :0.01166   Min.   :3.343e-05   Min.   :0.0005205   Min.   :0.0000   Min.   :0.0000
# 1st Qu.: 3.0   1st Qu.:0.4505   1st Qu.:0.35179   1st Qu.:1.060e-02   1st Qu.:0.1521234   1st Qu.:0.2400   1st Qu.:0.1300
# Median : 5.5   Median :0.5346   Median :0.43187   Median :1.918e-02   Median :0.3765368   Median :0.3400   Median :0.2200
# Mean   : 5.5   Mean   :0.5312   Mean   :0.43019   Mean   :2.575e-02   Mean   :0.4178495   Mean   :0.3371   Mean   :0.2195
# 3rd Qu.: 8.0   3rd Qu.:0.6109   3rd Qu.:0.51339   3rd Qu.:3.089e-02   3rd Qu.:0.6613907   3rd Qu.:0.4300   3rd Qu.:0.2900
# Max.   :10.0   Max.   :0.8391   Max.   :0.75638   Max.   :1.991e-01   Max.   :0.9974853   Max.   :0.6700   Max.   :0.6000
# aneuploid
# Min.   :0.1700
# 1st Qu.:0.3800
# Median :0.4400
# Mean   :0.4435
# 3rd Qu.:0.5200
# Max.   :0.6900
