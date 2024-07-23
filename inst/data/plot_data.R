# This file loads the generated data of meiotic and mitotic rates and plots them
# using different visualization methods.
if (!require(ggplot2))
  install.packages("ggplot2", repos = "http://cran.us.r-project.org")
library(ggplot2)
# if (!require(GGally))
#   install.packages("ggplot2", repos = "http://cran.us.r-project.org")
# library(GGally)
if (!require(readr))
  install.packages("readr", repos = "http://cran.us.r-project.org")
library(readr)
if (!require(gridExtra))
  install.packages("gridExtra", repos = "http://cran.us.r-project.org")
library(gridExtra)
if (!require(ggcorrplot))
  install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
library(ggcorrplot)

# -------------Load and Process Data-------------------------
# Locate the folder to investigate
date <- "2024-07-02"

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
colnames(my_data) <- my_data_cols[1, ]

# Read all the rest of the data
for (i in 2:100) {
  temp <-
    read_table(paste0("inst/data/", date, "/", i, ".txt"),
               skip = 1,
               col_names = FALSE)
  colnames(temp) <- my_data_cols[1, ]
  my_data <- rbind(my_data, temp)
}

# Combine two tables
# Locate the folder to investigate
date <- "2024-07-02"

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
colnames(new_data) <- new_data_cols[1, ]

# Read all the rest of the data
for (i in 2:100) {
  temp <-
    read_table(paste0("inst/data/", date, "/", i, ".txt"),
               skip = 1,
               col_names = FALSE)
  colnames(temp) <- new_data_cols[1, ]
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
h <- h + geom_histogram(
  binwidth = 0.1,
  color = "#000000",
  fill = "lightblue"
) + labs(x = "Probability of Meiotic Error", y = "Number of Embryos") +
  geom_vline(
    aes(xintercept = mean(my_data$prob.meio)),
    color = "red",
    linewidth = 1.25,
    linetype = "dashed"
  ) +
  # annotate(
  #   geom = "text",
  #   x = mean(my_data$prob.meio) - 0.25,
  #   y = 1200,
  #   fontface = "bold",
  #   label = paste("Average: ", round(mean(my_data$prob.meio), 2))
  # ) +
  scale_y_continuous(expand = c(0, 0)) + theme_classic()

# Mitotic
hm <-
  ggplot(data = my_data, aes(x = my_data$prob.mito))  +
  theme(
    axis.title.x = element_text(vjust = 0, size = 10, face = "bold"),
    axis.title.y = element_text(vjust = 2, size = 10, face = "bold")
  )
hm <- hm + geom_histogram(
  binwidth = 0.025,
  color = "#000000",
  fill = "lightblue",
) + labs(x = "Probability of Mitotic Error", y = "Number of Embryos") +
  geom_vline(
    aes(xintercept = mean(my_data$prob.mito)),
    color = "red",
    linewidth = 1.25,
    linetype = "dashed"
  ) +
  # annotate(
  #   geom = "text",
  #   x = mean(my_data$prob.mito) +0.1,
  #   y = 2000,
  #   fontface = "bold",
  #   label = paste("Average: ", round(mean(my_data$prob.mito), 2))
  # )   +
  scale_y_continuous(expand = c(0, 0)) + theme_classic()

# Two together

#
# hist(my_data$prob.mito, col='lightblue', xlab='Probability of Errors')
# hist(my_data$prob.meio, col='lightgreen', add=TRUE)
#
# #add legend
# legend('topright', c('mitotic errors', 'meiotic errors'), fill=c('lightblue', 'lightgreen'))

# Prop.aneu
ha <-
  ggplot(data = my_data, aes(x = my_data$prop.aneu))  +
  theme(
    axis.title.x = element_text(vjust = 0, size = 10, face = "bold"),
    axis.title.y = element_text(vjust = 2, size = 10, face = "bold")
  )
ha <- ha + geom_histogram(
  binwidth = 0.05,
  color = "#000000",
  fill = "lightblue",
) + labs(x = "Proprotion of Aneuploidy", y = "Number of Embryos") +
  geom_vline(
    aes(xintercept = mean(my_data$prop.aneu)),
    color = "red",
    linewidth = 1.25,
    linetype = "dashed"
  ) +
  # annotate(
  #   geom = "text",
  #   x = 0.35,
  #   y = 700,
  #   fontface = "bold",
  #   label = paste("Average: ", round(mean(my_data$prop.aneu), 2))
  # )   +
  scale_y_continuous(expand = c(0, 0)) + theme_classic()



#### Distribution of Dispersal ####
p <- ggplot(data = my_data, aes(x =  my_data$dispersal, )) + labs(x = "Probability of Mitotic Error", y = "Dispersal", color = "Dispersal")
p <- p + geom_histogram(
  binwidth = 0.15,
  color = "#000000",
  fill = "lightblue",
) + labs(x = "Dispersal", y = "Number of Embryos") +
  geom_vline(
    aes(xintercept = mean(my_data$dispersal)),
    color = "red",
    linewidth = 1.25,
    linetype = "dashed"
  ) +
  # annotate(
  #   geom = "text",
  #   x = 0.35,
  #   y = 400,
  #   fontface = "bold",
  #   label = paste("Average: ", round(mean(my_data$dispersal), 2))
  # )   +
  scale_y_continuous(expand = c(0, 0)) + theme_classic()



# Arrange the panel
grid.arrange(h, hm, ha, p)


#### Draw scatterplots ####
g <-
  ggplot(data = my_data, aes(x = my_data$prob.meio, y = my_data$prob.mito))
g <- g + geom_point(color = "sienna") + labs(x = "Probability of Meiotic Error", y = "Probability of Mitotic Error") +
  theme(
    axis.title.x = element_text(vjust = 0, size = 10, face = "bold"),
    axis.title.y = element_text(vjust = 2, size = 10, face = "bold")
  ) +
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
    color = "blue",
    linewidth = 1.25,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = mean(my_data$prob.meio),
    color = "blue",
    linewidth = 1.25,
    linetype = "dashed"
  )

# Scatterplots with dispersal
d <- ggplot(data = my_data,
            aes(
              x = my_data$prob.meio,
              y = my_data$prob.mito,
              color = my_data$dispersal
            ))

d <- d + geom_point() + labs(x = "Probability of Meiotic Error", y = "Probability of Mitotic Error", color = "Dispersal") +
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


# mitotic error vs dispersal
md <-
  ggplot(data = my_data, aes(x = my_data$dispersal, y = my_data$prob.mito))
md <- md + geom_point(color = "magenta") + labs(x = "Dispersal", y = "Probability of Mitotic Error") +
  theme(
    axis.title.x = element_text(vjust = 0, size = 10, face = "bold"),
    axis.title.y = element_text(vjust = 2, size = 10, face = "bold")
  ) +
  annotate(
    geom = "segment",
    x = quantile(my_data$dispersal, probs = c(.25)),
    xend = quantile(my_data$dispersal, probs = c(.75)),
    y = quantile(my_data$prob.mito, probs = c(.25)),
    yend = quantile(my_data$prob.mito, probs = c(.25)),
    color = "red",
    linewidth = 1
  ) + annotate(
    geom = "segment",
    x = quantile(my_data$dispersal, probs = c(.25)),
    xend = quantile(my_data$dispersal, probs = c(.75)),
    y = quantile(my_data$prob.mito, probs = c(.75)),
    yend = quantile(my_data$prob.mito, probs = c(.75)),
    color = "red",
    linewidth = 1
  ) + annotate(
    geom = "segment",
    x = quantile(my_data$dispersal, probs = c(.25)),
    xend = quantile(my_data$dispersal, probs = c(.25)),
    y = quantile(my_data$prob.mito, probs = c(.25)),
    yend = quantile(my_data$prob.mito, probs = c(.75)),
    color = "red",
    linewidth = 1
  ) + annotate(
    geom = "segment",
    x = quantile(my_data$dispersal, probs = c(.75)),
    xend = quantile(my_data$dispersal, probs = c(.75)),
    y = quantile(my_data$prob.mito, probs = c(.25)),
    yend = quantile(my_data$prob.mito, probs = c(.75)),
    color = "red",
    linewidth = 1
  ) +
  geom_hline(
    aes(yintercept = mean(my_data$prob.mito)),
    color = "blue",
    linewidth = 1.25,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = mean(my_data$dispersal),
    color = "blue",
    linewidth = 1.25,
    linetype = "dashed"
  )


# Arrange panel
grid.arrange(d, g)


#### Statistics ####

### Correlation ###
print(cor(my_data$prob.meio, my_data$prob.mito))

corm = cor(my_data[, c('prob.meio', 'prob.mito', 'dispersal')])
print(corm)

pmat = cor_pmat(my_data[, c('prob.meio', 'prob.mito', 'dispersal')])
print(pmat)

ggcorrplot(
  corm,
  type = "lower",
  lab = TRUE,
  p.mat = pmat,
  insig = "blank"
)

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

# summary for 1000 data in the smaller range (1st and 3rd quantiles), 4-4
# embryo       prop.aneu        prob.meio        prob.mito         dispersal
# Min.   : 1.0   Min.   :0.4532   Min.   :0.3601   Min.   :0.01102   Min.   :0.0003697
# 1st Qu.: 3.0   1st Qu.:0.4825   1st Qu.:0.3974   1st Qu.:0.01355   1st Qu.:0.1355719
# Median : 5.5   Median :0.4931   Median :0.4276   Median :0.01625   Median :0.3185236
# Mean   : 5.5   Mean   :0.4955   Mean   :0.4283   Mean   :0.01735   Mean   :0.3658146
# 3rd Qu.: 8.0   3rd Qu.:0.5071   3rd Qu.:0.4572   3rd Qu.:0.02033   3rd Qu.:0.5518917
# Max.   :10.0   Max.   :0.5600   Max.   :0.5099   Max.   :0.03100   Max.   :0.9993082
# euploid           mosaic         aneuploid
# Min.   :0.3700   Min.   :0.1700   Min.   :0.4100
# 1st Qu.:0.3800   1st Qu.:0.1800   1st Qu.:0.4200
# Median :0.3900   Median :0.1900   Median :0.4300
# Mean   :0.3874   Mean   :0.1867   Mean   :0.4258
# 3rd Qu.:0.3900   3rd Qu.:0.1900   3rd Qu.:0.4300
# Max.   :0.4000   Max.   :0.2000   Max.   :0.4400


# summary for 1000 data with 0 dispersal
# embryo       prop.aneu        prob.meio         prob.mito          dispersal    euploid
# Min.   : 1.0   Min.   :0.3527   Min.   :0.01037   Min.   :0.002648   Min.   :0   Min.   :0.0400
# 1st Qu.: 3.0   1st Qu.:0.4941   1st Qu.:0.32986   1st Qu.:0.024217   1st Qu.:0   1st Qu.:0.3000
# Median : 5.5   Median :0.5489   Median :0.38702   Median :0.035792   Median :0   Median :0.3600
# Mean   : 5.5   Mean   :0.5496   Mean   :0.38469   Mean   :0.039464   Mean   :0   Mean   :0.3574
# 3rd Qu.: 8.0   3rd Qu.:0.6035   3rd Qu.:0.44500   3rd Qu.:0.049736   3rd Qu.:0   3rd Qu.:0.4200
# Max.   :10.0   Max.   :0.7830   Max.   :0.67352   Max.   :0.170349   Max.   :0   Max.   :0.5600
# mosaic         aneuploid
# Min.   :0.0400   Min.   :0.2800
# 1st Qu.:0.1600   1st Qu.:0.4000
# Median :0.2100   Median :0.4300
# Mean   :0.2068   Mean   :0.4358
# 3rd Qu.:0.2500   3rd Qu.:0.4800
# Max.   :0.5400   Max.   :0.5900

# summary for 1000 data for dispersal 0.5
# embryo       prop.aneu        prob.meio         prob.mito           dispersal
# Min.   : 1.0   Min.   :0.2732   Min.   :0.04993   Min.   :0.0000711   Min.   :0.5
# 1st Qu.: 3.0   1st Qu.:0.4410   1st Qu.:0.36405   1st Qu.:0.0093711   1st Qu.:0.5
# Median : 5.5   Median :0.5278   Median :0.44853   Median :0.0170284   Median :0.5
# Mean   : 5.5   Mean   :0.5242   Mean   :0.44492   Mean   :0.0206510   Mean   :0.5
# 3rd Qu.: 8.0   3rd Qu.:0.6051   3rd Qu.:0.52790   3rd Qu.:0.0260281   3rd Qu.:0.5
# Max.   :10.0   Max.   :0.8406   Max.   :0.73187   Max.   :0.1991658   Max.   :0.5
# euploid           mosaic         aneuploid
# Min.   :0.0000   Min.   :0.0000   Min.   :0.2200
# 1st Qu.:0.2400   1st Qu.:0.1300   1st Qu.:0.3700
# Median :0.3300   Median :0.2100   Median :0.4500
# Mean   :0.3383   Mean   :0.2133   Mean   :0.4484
# 3rd Qu.:0.4400   3rd Qu.:0.2900   3rd Qu.:0.5300
# Max.   :0.6700   Max.   :0.5800   Max.   :0.7400


# summary for 1000 data points with dispersal 1
# embryo       prop.aneu        prob.meio          prob.mito           dispersal    euploid
# Min.   : 1.0   Min.   :0.2048   Min.   :0.003131   Min.   :5.961e-05   Min.   :1   Min.   :0.0000
# 1st Qu.: 3.0   1st Qu.:0.4270   1st Qu.:0.356467   1st Qu.:8.188e-03   1st Qu.:1   1st Qu.:0.2000
# Median : 5.5   Median :0.5180   Median :0.448093   Median :1.470e-02   Median :1   Median :0.3100
# Mean   : 5.5   Mean   :0.5181   Mean   :0.446774   Mean   :1.896e-02   Mean   :1   Mean   :0.3195
# 3rd Qu.: 8.0   3rd Qu.:0.6098   3rd Qu.:0.543985   3rd Qu.:2.263e-02   3rd Qu.:1   3rd Qu.:0.4300
# Max.   :10.0   Max.   :0.8368   Max.   :0.740339   Max.   :1.987e-01   Max.   :1   Max.   :0.7000
# mosaic         aneuploid
# Min.   :0.0000   Min.   :0.1400
# 1st Qu.:0.1400   1st Qu.:0.3700
# Median :0.2300   Median :0.4500
# Mean   :0.2297   Mean   :0.4508
# 3rd Qu.:0.3100   3rd Qu.:0.5400
# Max.   :0.6000   Max.   :0.7200

# correlation
# prob.meio  prob.mito  dispersal
# prob.meio  1.0000000 -0.2358313  0.1928092
# prob.mito -0.2358313  1.0000000 -0.6025071
# dispersal  0.1928092 -0.6025071  1.0000000

# correlation p-values
# prob.meio    prob.mito    dispersal
# prob.meio 0.000000e+00 4.991992e-77 9.478728e-05
# prob.mito 4.991992e-77 0.000000e+00 1.438761e-57
# dispersal 9.478728e-05 1.438761e-57 0.000000e+00



### Correct Biopsy ###
# sum(my_data$mosaic)*100/500000
# 0.218514
# sum(my_data$euploid)*100/500000
# 0.336616
# sum(my_data$aneuploid)*100/500000
# 0.44487
# 168308 euploid, 109257 mosaic, 222435 aneuploid (should all be mosaic)


#------For Paper-------

#### Figure 2 ####
# import dispersal_ranges.csv

# dispersal = 0
# Locate the folder to investigate
date <- "2024-07-09"

# Set column names
my_data_cols <-
  read_table(paste0("inst/data/", date, "/1.txt"),
             n_max  = 1,
             col_names = FALSE)
my_data_cols <- cbind('embryo', my_data_cols)

# Read the first txt file
disp_0 <-
  read_table(paste0("inst/data/", date, "/1.txt"),
             skip = 1,
             col_names = FALSE)
colnames(disp_0) <- my_data_cols[1, ]

# Read all the rest of the data
for (i in 2:100) {
  temp <-
    read_table(paste0("inst/data/", date, "/", i, ".txt"),
               skip = 1,
               col_names = FALSE)
  colnames(temp) <- my_data_cols[1, ]
  disp_0 <- rbind(disp_0, temp)
}

disp_0_meiotic <-
  ggplot(data = disp_0, aes(x = disp_0$prob.meio))  +
  theme(
    axis.title.x = element_text(vjust = 0, size = 10, face = "bold"),
    axis.title.y = element_text(vjust = 2, size = 10, face = "bold")
  )
disp_0_meiotic <- disp_0_meiotic + geom_histogram(
  binwidth = 0.1,
  color = "#000000",
  fill = "lightblue"
) + labs(x = element_blank(), y = "Number of Embryos") +
  geom_vline(
    aes(xintercept = mean(disp_0$prob.meio)),
    color = "red",
    linewidth = 1.25,
    linetype = "dashed"
  ) +
  scale_y_continuous(expand = c(0, 0)) + theme_classic()

disp_0_mitotic <-
  ggplot(data = disp_0, aes(x = disp_0$prob.mito))  +
  theme(
    axis.title.x = element_text(vjust = 0, size = 10, face = "bold"),
    axis.title.y = element_text(vjust = 2, size = 10, face = "bold")
  )
disp_0_mitotic <- disp_0_mitotic + geom_histogram(
  binwidth = 0.025,
  color = "#000000",
  fill = "lightblue",
) + labs(x = element_blank(), y = element_blank()) +
  geom_vline(
    aes(xintercept = mean(disp_0$prob.mito)),
    color = "red",
    linewidth = 1.25,
    linetype = "dashed"
  ) +
  scale_y_continuous(limits = c(0, 550), expand = c(0, 0)) +
  scale_x_continuous(limits = c(NA, 0.2)) + theme_classic()


# dispersal = 0.5
# Locate the folder to investigate
date <- "2024-07-07"

# Set column names
my_data_cols <-
  read_table(paste0("inst/data/", date, "/1.txt"),
             n_max  = 1,
             col_names = FALSE)
my_data_cols <- cbind('embryo', my_data_cols)

# Read the first txt file
disp_0.5 <-
  read_table(paste0("inst/data/", date, "/1.txt"),
             skip = 1,
             col_names = FALSE)
colnames(disp_0.5) <- my_data_cols[1, ]

# Read all the rest of the data
for (i in 2:100) {
  temp <-
    read_table(paste0("inst/data/", date, "/", i, ".txt"),
               skip = 1,
               col_names = FALSE)
  colnames(temp) <- my_data_cols[1, ]
  disp_0.5 <- rbind(disp_0.5, temp)
}

disp_0.5_meiotic <-
  ggplot(data = disp_0.5, aes(x = disp_0.5$prob.meio))  +
  theme(
    axis.title.x = element_text(vjust = 0, size = 10, face = "bold"),
    axis.title.y = element_text(vjust = 2, size = 10, face = "bold")
  )
disp_0.5_meiotic <- disp_0.5_meiotic + geom_histogram(
  binwidth = 0.1,
  color = "#000000",
  fill = "lightblue"
) + labs(x = element_blank(), y = "Number of Embryos") +
  geom_vline(
    aes(xintercept = mean(disp_0.5$prob.meio)),
    color = "red",
    linewidth = 1.25,
    linetype = "dashed"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, 450)) + theme_classic()

disp_0.5_mitotic <-
  ggplot(data = disp_0.5, aes(x = disp_0.5$prob.mito))  +
  theme(
    axis.title.x = element_text(vjust = 0, size = 10, face = "bold"),
    axis.title.y = element_text(vjust = 2, size = 10, face = "bold")
  )
disp_0.5_mitotic <- disp_0.5_mitotic + geom_histogram(
  binwidth = 0.025,
  color = "#000000",
  fill = "lightblue",
) + labs(x = element_blank(), y = element_blank()) +
  geom_vline(
    aes(xintercept = mean(disp_0.5$prob.mito)),
    color = "red",
    linewidth = 1.25,
    linetype = "dashed"
  ) +
  scale_y_continuous(expand = c(0, 0)) + theme_classic()

# dispersal = 1
# Locate the folder to investigate
date <- "2024-07-06"

# Set column names
my_data_cols <-
  read_table(paste0("inst/data/", date, "/1.txt"),
             n_max  = 1,
             col_names = FALSE)
my_data_cols <- cbind('embryo', my_data_cols)

# Read the first txt file
disp_1 <-
  read_table(paste0("inst/data/", date, "/1.txt"),
             skip = 1,
             col_names = FALSE)
colnames(disp_1) <- my_data_cols[1, ]

# Read all the rest of the data
for (i in 2:100) {
  temp <-
    read_table(paste0("inst/data/", date, "/", i, ".txt"),
               skip = 1,
               col_names = FALSE)
  colnames(temp) <- my_data_cols[1, ]
  disp_1 <- rbind(disp_1, temp)
}

disp_1_meiotic <-
  ggplot(data = disp_1, aes(x = disp_1$prob.meio))  +
  theme(
    axis.title.x = element_text(vjust = 0, size = 10, face = "bold"),
    axis.title.y = element_text(vjust = 2, size = 10, face = "bold")
  )
disp_1_meiotic <- disp_1_meiotic + geom_histogram(
  binwidth = 0.1,
  color = "#000000",
  fill = "lightblue"
) + labs(x = "Probability of Meiotic Error", y = "Number of Embryos") +
  geom_vline(
    aes(xintercept = mean(disp_1$prob.meio)),
    color = "red",
    linewidth = 1.25,
    linetype = "dashed"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, 450)) + theme_classic()

disp_1_mitotic <-
  ggplot(data = disp_1, aes(x = disp_1$prob.mito))  +
  theme(
    axis.title.x = element_text(vjust = 0, size = 10, face = "bold"),
    axis.title.y = element_text(vjust = 2, size = 10, face = "bold")
  )
disp_1_mitotic <- disp_1_mitotic + geom_histogram(
  binwidth = 0.025,
  color = "#000000",
  fill = "lightblue",
) + labs(x = "Probability of Mitotic Error", y = element_blank()) +
  geom_vline(
    aes(xintercept = mean(disp_1$prob.mito)),
    color = "red",
    linewidth = 1.25,
    linetype = "dashed"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, 550)) + theme_classic()

# Arrange the panel

grid.arrange(
  grobs = list(
    disp_0_meiotic,
    disp_0_mitotic,
    disp_0.5_meiotic,
    disp_0.5_mitotic,
    disp_1_meiotic,
    disp_1_mitotic
  ),
  top = "Probabilities of Meiotic and Mitotic Error at Different Dispersal Levels"
)


# Boxplot arrangement
library(readr)
dispersal_ranges <- read_csv("inst/data/dispersal_ranges.csv")
theme_set(theme_bw())
p1 <- ggplot(dispersal_ranges, aes(y = dispersal_ranges$prob.meio)) +
  geom_boxplot() +
  facet_wrap(~ dispersal) + labs(y = "Probability of Meiotic Error") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
# one box per variety
p2 <- ggplot(dispersal_ranges, aes(y = dispersal_ranges$prob.mito)) +
  geom_boxplot() +
  facet_wrap(~ dispersal) + labs(y = "Probability of Mitotic Error", x =
                                   "Dispersal") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

grid.arrange(p1, p2)


#### Figure 3 ####

# Read data
# dispersal = 0
# Locate the folder to investigate
date <- "2024-07-20"

# Set column names
my_data_cols <-
  read_table(paste0("inst/data/", date, "/1.txt"),
             n_max  = 1,
             col_names = FALSE)
my_data_cols <- cbind('embryo', my_data_cols)

# Read the first txt file
prop_disp_0 <-
  read_table(paste0("inst/data/", date, "/1.txt"),
             skip = 1,
             col_names = FALSE)
colnames(prop_disp_0) <- my_data_cols[1, ]

# Read all the rest of the data
for (i in 2:100) {
  temp <-
    read_table(paste0("inst/data/", date, "/", i, ".txt"),
               skip = 1,
               col_names = FALSE)
  colnames(temp) <- my_data_cols[1, ]
  prop_disp_0 <- rbind(prop_disp_0, temp)
}

prop_aneu_disp_0 <-
  ggplot(data = prop_disp_0, aes(x = prop_disp_0$prop.aneu))  +
  theme(
    axis.title.x = element_text(vjust = 0, size = 10, face = "bold"),
    axis.title.y = element_text(vjust = 2, size = 10, face = "bold")
  )
prop_aneu_disp_0 <- prop_aneu_disp_0 + geom_histogram(
  binwidth = 0.05,
  color = "#000000",
  fill = "lightblue",
) + labs(x = "Proprotion of Aneuploidy", y = "Number of Embryos") +
  geom_vline(
    aes(xintercept = mean(prop_disp_0$prop.aneu)),
    color = "red",
    linewidth = 1.25,
    linetype = "dashed"
  ) +
  # annotate(
  #   geom = "text",
  #   x = 0.35,
  #   y = 700,
  #   fontface = "bold",
  #   label = paste("Average: ", round(mean(my_data$prop.aneu), 2))
  # )   +
  scale_y_continuous(expand = c(0, 0)) + theme_classic()

# Classification

# Create the data for the chart
A <- c(sum(prop_disp_0$prop.aneu < 0.2),
       sum((prop_disp_0$prop.aneu >= 0.2) &
             (prop_disp_0$prop.aneu < 0.8)),
       sum(prop_disp_0$prop.aneu >= 0.8))
# 28742, 32352, 38906
B <- c("Euploid", "Mosaic", "Aneuploid")

# Plot the bar chart with text features
barplot(
  A,
  names.arg = B,
  xlab = "Embryo Type",
  ylab = "Number of Embryos",
  col = "steelblue"
)

# Add data labels on top of each bar
# text(
#   x = barplot(A, names.arg = B, col = "steelblue", ylim = c(0, max(A) * 1.2)),
#   y = A + 1, labels = A, pos = 3, cex = 1.2, col = "black"
# )

#### Table 1 ####
# using dispersal data from above
# dispersal = 0
# Locate the folder to investigate
date <- "2024-07-09"

# Set column names
my_data_cols <-
  read_table(paste0("inst/data/", date, "/1.txt"),
             n_max  = 1,
             col_names = FALSE)
my_data_cols <- cbind('embryo', my_data_cols)

# Read the first txt file
disp_0 <-
  read_table(paste0("inst/data/", date, "/1.txt"),
             skip = 1,
             col_names = FALSE)
colnames(disp_0) <- my_data_cols[1, ]

# Read all the rest of the data
for (i in 2:100) {
  temp <-
    read_table(paste0("inst/data/", date, "/", i, ".txt"),
               skip = 1,
               col_names = FALSE)
  colnames(temp) <- my_data_cols[1, ]
  disp_0 <- rbind(disp_0, temp)
}

# dispersal = 0.5
# Locate the folder to investigate
date <- "2024-07-07"

# Set column names
my_data_cols <-
  read_table(paste0("inst/data/", date, "/1.txt"),
             n_max  = 1,
             col_names = FALSE)
my_data_cols <- cbind('embryo', my_data_cols)

# Read the first txt file
disp_0.5 <-
  read_table(paste0("inst/data/", date, "/1.txt"),
             skip = 1,
             col_names = FALSE)
colnames(disp_0.5) <- my_data_cols[1, ]

# Read all the rest of the data
for (i in 2:100) {
  temp <-
    read_table(paste0("inst/data/", date, "/", i, ".txt"),
               skip = 1,
               col_names = FALSE)
  colnames(temp) <- my_data_cols[1, ]
  disp_0.5 <- rbind(disp_0.5, temp)
}

# dispersal = 1
# Locate the folder to investigate
date <- "2024-07-06"

# Set column names
my_data_cols <-
  read_table(paste0("inst/data/", date, "/1.txt"),
             n_max  = 1,
             col_names = FALSE)
my_data_cols <- cbind('embryo', my_data_cols)

# Read the first txt file
disp_1 <-
  read_table(paste0("inst/data/", date, "/1.txt"),
             skip = 1,
             col_names = FALSE)
colnames(disp_1) <- my_data_cols[1, ]

# Read all the rest of the data
for (i in 2:100) {
  temp <-
    read_table(paste0("inst/data/", date, "/", i, ".txt"),
               skip = 1,
               col_names = FALSE)
  colnames(temp) <- my_data_cols[1, ]
  disp_1 <- rbind(disp_1, temp)
}

library(vtable)
st(disp_0[,c('prob.meio','prob.mito','euploid', 'mosaic','aneuploid')])
st(disp_0.5[,c('prob.meio','prob.mito','euploid', 'mosaic','aneuploid')])
st(disp_1[,c('prob.meio','prob.mito','euploid', 'mosaic','aneuploid')])
