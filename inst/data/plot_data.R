# This file loads the generated data of meiotic and mitotic rates and plots them
# using different visualization methods.
if (!require(ggplot2))
  install.packages("ggplot2", repos = "http://cran.us.r-project.org")
library(ggplot2)
library(GGally)
# if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
library(readr)

# -------------Load and Process Data-------------------------
# Locate the folder to investigate
date <- "2024-03-28"

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
date <- "2024-02-12"

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

my_data <- rbind(new_data, my_data)

# -------------Graphing-------------------------
theme_set(theme_bw())

#### Draw Histograms ####
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
    y = 1200,
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
    x = mean(my_data$prob.mito) +0.1,
    y = 2000,
    fontface = "bold",
    label = paste("Average: ", round(mean(my_data$prob.mito), 2))
  )   + scale_y_continuous(expand = c(0,0)) + theme_classic()

# Two together


hist(my_data$prob.mito, col='lightblue', xlab='Probability of Errors')
hist(my_data$prob.meio, col='lightgreen', add=TRUE)

#add legend
legend('topright', c('mitotic errors', 'meiotic errors'), fill=c('lightblue', 'lightgreen'))

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
    y = 700,
    fontface = "bold",
    label = paste("Average: ", round(mean(my_data$prop.aneu), 2))
  )   + scale_y_continuous(expand = c(0,0)) + theme_classic()



#### Draw scatterplots ####
g <-
  ggplot(data = my_data, aes(x = my_data$prob.meio, y = my_data$prob.mito))
g + geom_point(color = "sienna") + labs(x = "Probability of Meiotic Errors",
                                        y = "Probability of Mitotic Errors") +
  theme(
    axis.title.x = element_text(vjust = 0, size = 10, face = "bold"),
    axis.title.y = element_text(vjust = 2, size = 10, face = "bold")
  )+
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
  ) +
  geom_hline(
    aes(yintercept = mean(my_data$prob.mito)),
    color = "red",
    linewidth = 1.25,
    linetype = "dashed"
  ) +
  geom_vline(
    aes(xintercept = mean(my_data$prob.meio),
    color = "red",
    linewidth = 1.25,
    linetype = "dashed"
  ))

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


#### Distribution of Dispersal ####
p <- ggplot(data = my_data,
            aes(
              x =  my_data$dispersal,
            )) + labs(x = "Probability of Mitotic Errors",
                      y = "Dispersal",
                      color = "Dispersal")
p + geom_histogram(
  binwidth = 0.15,
  color = "#000000",
  fill = "lightblue",
) + labs(x = "Dispersal",
         y = "Count") +
  geom_vline(
    aes(xintercept = mean(my_data$dispersal)),
    color = "red",
    linewidth = 1.25,
    linetype = "dashed"
  ) +
  annotate(
    geom = "text",
    x = 0.35,
    y = 400,
    fontface = "bold",
    label = paste("Average: ", round(mean(my_data$dispersal), 2))
  )   + scale_y_continuous(expand = c(0,0)) + theme_classic()

p + geom_violin(fill = "gray80",
                linewidth = 1,
                alpha = .5)

#### Correlation ####
pair <- ggpairs(my_data[,4:6]) + theme_minimal()
pair

library("ggcorrplot")
# Compute a correlation matrix
corr <- round(cor(my_data[,4:6]), 1)
# Visualize
ggcorrplot(corr, p.mat = cor_pmat(my_data[,4:6]),
           hc.order = TRUE, type = "lower",
           color = c("#FC4E07", "white", "#00AFBB"),
           outline.col = "white", lab = TRUE)

#### Boxplot ####
ggplot(my_data, aes(y = my_data[,4:6])) +
  geom_boxplot(fill = "indianred", orientation = "y") +
  labs(x = "Values", y = "Variables")


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

# summary for 5000 data with dispersal and 0.01 tolerance
# ...1          embryo       prop.aneu        prob.meio          prob.mito           dispersal            euploid
# Min.   :   1   Min.   : 1.0   Min.   :0.2271   Min.   :0.001703   Min.   :1.479e-05   Min.   :0.0000493   Min.   :0.0000
# 1st Qu.:1251   1st Qu.: 3.0   1st Qu.:0.4517   1st Qu.:0.356319   1st Qu.:1.076e-02   1st Qu.:0.1546677   1st Qu.:0.2400
# Median :2500   Median : 5.5   Median :0.5350   Median :0.435587   Median :1.897e-02   Median :0.3770824   Median :0.3300
# Mean   :2500   Mean   : 5.5   Mean   :0.5315   Mean   :0.432737   Mean   :2.536e-02   Mean   :0.4180395   Mean   :0.3366
# 3rd Qu.:3750   3rd Qu.: 8.0   3rd Qu.:0.6090   3rd Qu.:0.513566   3rd Qu.:3.059e-02   3rd Qu.:0.6638928   3rd Qu.:0.4300
# Max.   :5000   Max.   :10.0   Max.   :0.8603   Max.   :0.761285   Max.   :2.187e-01   Max.   :0.9998474   Max.   :0.6700
# mosaic         aneuploid
# Min.   :0.0000   Min.   :0.1700
# 1st Qu.:0.1400   1st Qu.:0.3800
# Median :0.2200   Median :0.4500
# Mean   :0.2185   Mean   :0.4449
# 3rd Qu.:0.2900   3rd Qu.:0.5200
# Max.   :0.6100   Max.   :0.6900
