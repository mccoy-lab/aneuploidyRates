# This file loads the generated data of meiotic and mitotic rates and plots them
# using different visualization methods.
if (!require(ggplot2))
  install.packages("ggplot2", repos = "http://cran.us.r-project.org")
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


# -------------Graphing-------------------------
theme_set(theme_bw())

# Draw Histograms
h <-
  ggplot(data = my_data, aes(x = my_data$prob.meio))  +
  theme(
    axis.title.x = element_text(vjust = 0, size = 10, face = "bold"),
    axis.title.y = element_text(vjust = 2, size = 10, face = "bold")
  )
h + geom_histogram(
  binwidth = 0.1,
  color = "#000000",
  fill = "lightblue"
) + labs(x = "Probability of Meiotic Errors",
         y = "Count") +
  geom_vline(
    aes(xintercept = mean(my_data$prob.meio)),
    color = "red",
    linewidth = 1.25,
    linetype = "dashed"
  ) +
  annotate(
    geom = "text",
    x = mean(my_data$prob.meio) - 0.25,
    y = 250,
    fontface = "bold",
    label = paste("Average: ", round(mean(my_data$prob.meio), 2))
  ) + scale_y_continuous(expand = c(0,0)) + theme_classic()

# Mitotic
hm <-
  ggplot(data = my_data, aes(x = my_data$prob.mito))  +
  theme(
    axis.title.x = element_text(vjust = 0, size = 10, face = "bold"),
    axis.title.y = element_text(vjust = 2, size = 10, face = "bold")
  )
hm + geom_histogram(
  binwidth = 0.025,
  color = "#000000",
  fill = "lightblue",
) + labs(x = "Probability of Mitotic Errors",
         y = "Count") +
  geom_vline(
    aes(xintercept = mean(my_data$prob.mito)),
    color = "red",
    linewidth = 1.25,
    linetype = "dashed"
  ) +
  annotate(
    geom = "text",
    x = mean(my_data$prob.mito) +0.05,
    y = 400,
    fontface = "bold",
    label = paste("Average: ", round(mean(my_data$prob.mito), 2))
  )   + scale_y_continuous(expand = c(0,0)) + theme_classic()

# Prop.aneu
ha <-
  ggplot(data = my_data, aes(x = my_data$prop.aneu))  +
  theme(
    axis.title.x = element_text(vjust = 0, size = 10, face = "bold"),
    axis.title.y = element_text(vjust = 2, size = 10, face = "bold")
  )
ha + geom_histogram(
  binwidth = 0.05,
  color = "#000000",
  fill = "lightblue",
) + labs(x = "Proprotion of Aneuploidy",
         y = "Count") +
  geom_vline(
    aes(xintercept = mean(my_data$prop.aneu)),
    color = "red",
    linewidth = 1.25,
    linetype = "dashed"
  ) +
  annotate(
    geom = "text",
    x = 0.35,
    y = 140,
    fontface = "bold",
    label = paste("Average: ", round(mean(my_data$prop.aneu), 2))
  )   + scale_y_continuous(expand = c(0,0)) + theme_classic()



# Draw scatterplots
g <-
  ggplot(data = my_data, aes(x = my_data$prob.meio, y = my_data$prob.mito))
g + geom_point(color = "sienna") + labs(x = "Probability of Meiotic Errors",
                                        y = "Probability of Mitotic Errors") +
  theme(
    axis.title.x = element_text(vjust = 0, size = 10, face = "bold"),
    axis.title.y = element_text(vjust = 2, size = 10, face = "bold")
  )

# Scatterplots with dispersal
d <- ggplot(data = my_data,
            aes(
              x = my_data$prob.meio,
              y = my_data$prob.mito,
              color = my_data$dispersal
            ))

d + geom_point() + labs(x = "Probability of Meiotic Errors",
                        y = "Probability of Mitotic Errors",
                        color = "Dispersal") +
  theme(
    axis.title.x = element_text(vjust = 0, size = 10, face = "bold"),
    axis.title.y = element_text(vjust = 2, size = 10, face = "bold"),
    legend.position = c(.87, .8),
    legend.background = element_rect(fill = "transparent"),
    panel.grid = element_blank()
  ) +
  guides(color = guide_colorsteps())  + scale_color_viridis_c() +   geom_rug() +
  annotate(
    geom = "segment",
    x = quantile(my_data$prob.meio, probs = c(.25)),
    xend = quantile(my_data$prob.meio, probs = c(.75)),
    y = quantile(my_data$prob.mito, probs = c(.25)),
    yend = quantile(my_data$prob.mito, probs = c(.25)),
    color = "red",
    linewidth = 1
  ) + annotate(
    geom = "segment",
    x = quantile(my_data$prob.meio, probs = c(.25)),
    xend = quantile(my_data$prob.meio, probs = c(.75)),
    y = quantile(my_data$prob.mito, probs = c(.75)),
    yend = quantile(my_data$prob.mito, probs = c(.75)),
    color = "red",
    linewidth = 1
  ) + annotate(
    geom = "segment",
    x = quantile(my_data$prob.meio, probs = c(.25)),
    xend = quantile(my_data$prob.meio, probs = c(.25)),
    y = quantile(my_data$prob.mito, probs = c(.25)),
    yend = quantile(my_data$prob.mito, probs = c(.75)),
    color = "red",
    linewidth = 1
  ) + annotate(
    geom = "segment",
    x = quantile(my_data$prob.meio, probs = c(.75)),
    xend = quantile(my_data$prob.meio, probs = c(.75)),
    y = quantile(my_data$prob.mito, probs = c(.25)),
    yend = quantile(my_data$prob.mito, probs = c(.75)),
    color = "red",
    linewidth = 1
  )


# Distribution of Dispersal
p <- ggplot(data = my_data,
            aes(
              x = my_data$prob.mito,
              y = my_data$dispersal,
              color = my_data$dispersal
            )) + labs(x = "Probability of Mitotic Errors",
                      y = "Dispersal",
                      color = "Dispersal")
p + geom_violin(fill = "gray80",
                linewidth = 1,
                alpha = .5)



#
# # Draw scatterplots
ggplot(data = new_data, aes(x = new_data$prob.meio, y = new_data$prob.mito)) + geom_point()
ggplot(data = new_data,
       aes(
         x = new_data$prob.meio,
         y = new_data$prob.mito,
         color = new_data$dispersal
       )) + geom_point()
#
#
result <- rbind(new_data, my_data)
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
