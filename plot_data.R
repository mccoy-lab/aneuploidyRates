# This file loads the generated data of meiotic and mitotic rates and plots them
# using different visualization methods.

# To start with, load all the packages right below.

# All figures for the paper are under each specific section titles. 


## Relevant data info in the data folder ##

# Currently in use:

# 08-30 -- generated embryos based on distributions in 08-29

# 08-29 -- misdiagnosed rates applied in expected values, dispersal 0, 0.5, 1

# 08-23c, d, e -- 3000 ABC_seq Lenormand data for Munne 2017

# 08-17c, d, e -- 3000 ABC_seq Lenormand data for Capalbo

# 08-16c, d, e -- 3000 ABC_seq Lenormand data for Viotti

#------For Paper-----------------------------------------------------
if (!require(dplyr))
  install.packages("dplyr", repos = "http://cran.us.r-project.org")
library(dplyr)
if (!require(bayestestR)) {
  install.packages("bayestestR")
}
library(bayestestR)
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
if (!require(ggpubr))
  install.packages("ggpubr", repos = "http://cran.us.r-project.org")
library(ggpubr)
library(ggplot2)
if (!require(viridis))
  install.packages("viridis", repos = "http://cran.us.r-project.org")
library(viridis)
if (!require(patchwork)) {
  install.packages("patchwork")
}
library(patchwork)
if (!require("gt")) {
  install.packages("gt")
  library(gt)
}
if (!require("kableExtra")) {
  install.packages("kableExtra")
  library(kableExtra)
}
if (!require("vtable")) {
  install.packages("vtable")
  library(vtable)
}
if (!require(reshape2)) {
  install.packages("reshape2")
}
library(reshape2)
#### Figure 3 #############################################################

data1 <- read.csv("data/2024-08-16c/data.csv")
data2 <- read.csv("data/2024-08-16d/data.csv")
data3 <- read.csv("data/2024-08-16e/data.csv")
dispersal_ranges <- rbind(data1, data2, data3)

# Together
library(reshape2)
data_melt <- melt(
  dispersal_ranges,
  id.vars = c("dispersal", "euploid", "mosaic", "aneuploid"),
  measure.vars = c("prob.meio", "prob.mito")
)

variable_labels <- c(prob.meio = "Probability of Meiotic Error", prob.mito = "Probability of Mitotic Error")


max_estimates <- data_melt %>%
  group_by(dispersal, variable) %>%
  summarise(map_estimate(value)[2])


# Plot the histograms
ggplot(data_melt, aes(x = value)) +
  facet_grid(
    dispersal ~ variable,
    scales = "free_x",
    labeller = labeller(variable = variable_labels)
  ) +
  geom_histogram(
    data = subset(data_melt, variable == "prob.meio"),
    binwidth = 0.005,
    boundary = 0,
    fill = "steelblue",
    color = "black"
  ) +
  geom_histogram(
    data = subset(data_melt, variable == "prob.mito"),
    binwidth = 0.001,
    boundary = 0,
    fill = "steelblue",
    color = "black"
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    sec.axis = sec_axis(
      ~ . ,
      name = "Dispersal",
      breaks = NULL,
      labels = NULL
    )
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Error Rates", y = "Number of Embryos") +
  geom_vline(
    data = max_estimates,
    aes(xintercept = MAP_Estimate),
    color = "red",
    linewidth = 0.75,
    linetype = "dashed"
  ) +
  theme_bw()


#### Figure 4 ##################################################

# Read prop.aneu data to create dispersal_ranges
data1 <- read.csv("data/2024-08-16c/full_data.csv")
data2 <- read.csv("data/2024-08-16d/full_data.csv")
data3 <- read.csv("data/2024-08-16e/full_data.csv")
dispersal_ranges <- rbind(data1, data2, data3)

# By cell (bar at 0% represents the number of euploid embryos only)
euploid_heights <- dispersal_ranges %>%
  group_by(dispersal) %>%
  summarise(euploid_height = (sum(prop.aneu == 0) / n()))

total_count <- sum(dispersal_ranges$dispersal == 0)

prop.hist <- ggplot(dispersal_ranges, aes(x = prop.aneu)) +
  facet_grid(rows = vars(factor(dispersal, levels = c("0", "0.5", "1"))), scales = "fixed") +
  geom_histogram(
    data = dispersal_ranges,
    aes(y = ..count.. / total_count),
    binwidth = 1 / 257,
    boundary = 0,
    fill = "red",
    color = "black"
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(
    expand = c(0, 0),
    sec.axis = sec_axis(
      ~ . ,
      name = "Dispersal",
      breaks = NULL,
      labels = NULL
    ),
    labels = scales::percent_format()
  ) +
  labs(x = "Proportion of Aneuploidy", y = "Percentage of Embryos", tag = "A") +
  theme_bw() +
  geom_segment(
    data = euploid_heights,
    aes(
      x = 0.1,
      xend = 0,
      y = euploid_height + 0.25,
      yend = euploid_height
    ),
    arrow = arrow(length = unit(0.3, 'cm')),
    size = 0.5,
    color = "red"
  ) +
  geom_text(
    data = euploid_heights,
    aes(
      x = 0.1,
      y = euploid_height + 0.25,
      label = sprintf("%.1f%%", euploid_height * 100)
    ),
    vjust = -0.5,
    hjust = -0.1,
    fontface = 'bold',
    size = 4.5,
    show.legend = FALSE
  )


###### ggplot barplot

biopsy_data <- dispersal_ranges %>%
  mutate(
    category = case_when(
      prop.aneu == 0 ~ "Euploid",
      prop.aneu > 0 & prop.aneu < 1 ~ "Mosaic Aneuploid",
      prop.aneu == 1 ~ "Fully Aneuploid"
    )
  )

embryo_types <- biopsy_data %>%
  group_by(dispersal, prob.meio, prob.mito, category) %>%
  summarise(count = n()) %>%
  mutate(label_ypos = cumsum(count) + 0.5) %>%
  mutate(percent = count / sum(count) * 100)

embryo_types$category <- factor(embryo_types$category,
                                levels = c("Euploid", "Mosaic Aneuploid", "Fully Aneuploid"))

# calculate mean and standard deviations
embryo_sum <- embryo_types %>%
  group_by(dispersal, category) %>%
  summarize(mean = mean(percent), std = sd(percent)) %>%
  mutate(xpos = c(12, 40, 80)) %>%
  mutate(new_mean = cumsum(mean))

# percentages
# Horizontal percentage bar chart
percent.bar <- ggplot(embryo_sum, aes(
  x = factor(dispersal, levels = c(1, 0.5, 0)),
  y = mean,
  fill = factor(
    category,
    levels = c("Fully Aneuploid", "Mosaic Aneuploid", "Euploid")
  )
)) +
  geom_bar(stat = "identity") +
  labs(x = "Dispersal",
       y = "Percentage of Embryos",
       fill = "Embryo Type",
       tag = "B") +
  geom_label(
    aes(y = xpos, label = sprintf("%.1f%%", mean)),
    color = "red",
    fill = "white",
    fontface = "bold",
    size = 4
  ) +
  scale_fill_viridis(discrete = TRUE) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() + coord_flip()


# Plot the single stacked bar chart

data <- data.frame(
  category = c("Euploid", "Mosaic", "Aneuploid"),
  value = c(0.388, 0.186, 0.426)
)

data <- data %>%
  mutate(ypos = c(0.20, 0.50, 0.80))

ref <- ggplot(data, aes(x = 1, y = value, fill = factor(
  category,
  levels = c("Aneuploid", "Mosaic", "Euploid")
))) + 
  geom_bar(stat = "identity", width = 0.5) +
  labs(x = "", y = "Percentage", fill = "Biopsy Type") + 
  ggtitle("Reference Proportions from Viotti et al. 2021")  +
  geom_label(
    aes(y = ypos, label = sprintf("%.1f%%", value*100)),
    color = "red",
    fill = "white",
    fontface = "bold",
    size = 4
  )   +
  scale_fill_viridis(discrete = TRUE) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_void()+
  theme(    axis.text = element_blank(),         # Hide text on both axes
            )+ coord_flip()

layout <- "
AABB
AACC
AACC
AACC
"
prop.hist + ref + percent.bar +
  plot_layout(design = layout)


#### Table S1 & S2 #########################################################

data1 <- read.csv("data/2024-08-16c/data.csv")
data2 <- read.csv("data/2024-08-16d/data.csv")
data3 <- read.csv("data/2024-08-16e/data.csv")
dispersal_ranges <- rbind(data1, data2, data3)

disp_0 <- subset(dispersal_ranges, dispersal == 0)
disp_0.5 <- subset(dispersal_ranges, dispersal == 0.5)
disp_1 <- subset(dispersal_ranges, dispersal == 1)

stats_0 <- st(disp_0[, c('prob.meio', 'prob.mito')], out = "return", 
              summ = c('mean(x)','pctile(x)[2.5]', 'pctile(x)[25]', 'median(x)', 'pctile(x)[75]','pctile(x)[97.5]','max(x)'))
stats_0 <- t(stats_0)
stats_0.5 <- st(disp_0.5[, c('prob.meio', 'prob.mito')], out = "return", 
                summ = c('mean(x)','pctile(x)[2.5]', 'pctile(x)[25]', 'median(x)','pctile(x)[75]','pctile(x)[97.5]','max(x)'))
stats_0.5 <- t(stats_0.5)
stats_1 <- st(disp_1[, c('prob.meio', 'prob.mito')], out = "return", 
              summ = c('mean(x)','pctile(x)[2.5]', 'pctile(x)[25]', 'median(x)','pctile(x)[75]','pctile(x)[97.5]','max(x)'))
stats_1 <- t(stats_1)
stats_sum <- (cbind(stats_0, stats_0.5, stats_1))

# Add MAP and remove redundant rows
stats_sum <- stats_sum[!(row.names(stats_sum) %in% c("N", "Std. Dev.", "Min", "Max")), ]

data_melt <- melt(
  dispersal_ranges,
  id.vars = c("dispersal", "euploid", "mosaic", "aneuploid"),
  measure.vars = c("prob.meio", "prob.mito")
)

variable_labels <- c(prob.meio = "Probability of Meiotic Error", prob.mito = "Probability of Mitotic Error")

max_estimates <- data_melt %>%
  group_by(dispersal, variable) %>%
  summarise(map_estimate(value)[2])

stats_sum <- rbind(
  c("Dispersal 0", "", "Dispersal 0.5", "", "Dispersal 1", ""),
  stats_sum,
  MAP = signif(max_estimates$MAP_Estimate, 2)
)

kbl(stats_sum, format = "markdown")


stats_0 <- st(disp_0[, c('euploid', 'mosaic', 'aneuploid')], out = "return")
stats_0 <- t(stats_0)
stats_0.5 <- st(disp_0.5[, c('euploid', 'mosaic', 'aneuploid')], out = "return")
stats_0.5 <- t(stats_0.5)
stats_1 <- st(disp_1[, c('euploid', 'mosaic', 'aneuploid')], out = "return")
stats_1 <- t(stats_1)

# Calculate medians for euploid, mosaic, and aneuploid columns
median_0 <- apply(disp_0[, c('euploid', 'mosaic', 'aneuploid')], 2, median, na.rm = TRUE)
median_0.5 <- apply(disp_0.5[, c('euploid', 'mosaic', 'aneuploid')], 2, median, na.rm = TRUE)
median_1 <- apply(disp_1[, c('euploid', 'mosaic', 'aneuploid')], 2, median, na.rm = TRUE)

# Add medians to the stats data
stats_sum <- cbind(stats_0, stats_0.5, stats_1)


# Insert the median row
stats_sum <- rbind(
  stats_sum[1:6, ],
  "Pctl. 50" = c(signif(median_0, 2), signif(median_0.5, 2), signif(median_1, 2)),
  stats_sum[7:nrow(stats_sum), ]
)

stats_sum <- rbind(c("Dispersal 0", "", "", "Dispersal 0.5", "", "", "Dispersal 1", "", ""),
                   stats_sum)
kbl(stats_sum, format = "markdown")

#### Figure 2 ###################################
# import dispersal_ranges
data1 <- read.csv("data/2024-08-16c/data.csv")
data2 <- read.csv("data/2024-08-16d/data.csv")
data3 <- read.csv("data/2024-08-16e/data.csv")
dispersal_ranges <- rbind(data1, data2, data3)


# Euclidean distance in the same plot
dispersal_ranges <- dispersal_ranges %>% mutate(euclidean = sqrt((euploid - 0.388) ^ 2 + (mosaic - 0.188) ^ 2 +
                                                                   (aneuploid - 0.426) ^ 2))
ggplot(data = dispersal_ranges, aes(x = prob.meio, y = prob.mito, color = euclidean)) +
  geom_point(size = 1) + facet_grid(dispersal ~ .,
                                    scales = "fixed") + 
  stat_cor(method = "pearson", label.x = 0.35, label.y = 0.019) +
  labs(
    x = "Probability of Meiotic Error",
    y = "Probability of Mitotic Error",
    color = "Distance",
    shape = "Dispersal"
  ) +
  theme(
    axis.title.x = element_text(vjust = 0, size = 10, face = "bold"),
    axis.title.y = element_text(vjust = 2, size = 10, face = "bold"),
    legend.position = c(.87, .8),
    legend.background = element_rect(fill = "transparent"),
    panel.grid = element_blank()
  ) + scale_y_continuous(sec.axis = sec_axis(
    ~ . ,
    name = "Dispersal",
    breaks = NULL,
    labels = NULL
  )) +
  guides(color = guide_colorsteps())  + scale_color_viridis_c(oob = scales::squish) + geom_rug() +
  theme_bw()

# Plotting with weights
dispersal_ranges <- dispersal_ranges %>% mutate(euclidean = sqrt((euploid - 0.388) ^ 2 + (mosaic - 0.188) ^ 2 +
                                                                   (aneuploid - 0.426) ^ 2))
ggplot(data = dispersal_ranges, aes(x = prob.meio, y = prob.mito, size = weights, color = euclidean)) +
  geom_point() + facet_grid(dispersal ~ .,
                                    scales = "fixed") +
  labs(
    x = "Probability of Meiotic Error",
    y = "Probability of Mitotic Error",
    size = "Weights",
    color = "Distance",
    shape = "Dispersal"
  ) +
  theme(
    axis.title.x = element_text(vjust = 0, size = 10, face = "bold"),
    axis.title.y = element_text(vjust = 2, size = 10, face = "bold"),
    legend.position = c(.87, .8),
    legend.background = element_rect(fill = "transparent"),
    panel.grid = element_blank()
  ) + scale_y_continuous(sec.axis = sec_axis(
    ~ . ,
    name = "Dispersal",
    breaks = NULL,
    labels = NULL
  )) +
  guides(color = guide_colorsteps())  + scale_color_viridis_c(oob = scales::squish) + geom_rug() +
  theme_bw() + scale_size_area(max_size = 2)


#### Figure 5 ##############
date <- "2024-08-30"
data <- c()
for(i in 1:11) {
  new_data <- read.csv(paste0("data/", date, "/data_" , i, ".csv"))
  data <- rbind(data, new_data)
}
data <- cbind(data, dispersal = 0)

date <- "2024-08-30b"
for(i in 1:11) {
  new_data <- read.csv(paste0("data/", date, "/data_" , i, ".csv"))
  new_data <- cbind(new_data, dispersal = 0.5)
  data <- rbind(data, new_data)
}

date <- "2024-08-30c"
for(i in 1:11) {
  new_data <- read.csv(paste0("data/", date, "/data_" , i, ".csv"))
  new_data <- cbind(new_data, dispersal = 1)
  data <- rbind(data, new_data)
}

data$Mosaic.Aneuploid <- data$Mosaic.Aneuploid / 1000
mosaic_data <- data %>%  group_by(misclassification, dispersal) %>%
  summarise(proportion =mean(Mosaic.Aneuploid), stdev = sd(Mosaic.Aneuploid))

ggplot(data = mosaic_data, aes(x = misclassification, y = proportion)) +
  geom_point(size = 3) +
  facet_grid(dispersal ~ .,
                                    scales = "fixed") +
  labs(x = "Biopsy Misclassification Rate", y = "Proportion of Mosaic Aneuploidy Embryos", ) +
  theme(
    axis.title.x = element_text(vjust = 0, size = 10, face = "bold"),
    axis.title.y = element_text(vjust = 2, size = 10, face = "bold"),
    legend.position = c(.87, .8),
    legend.background = element_rect(fill = "transparent"),
    panel.grid = element_blank()
  ) +
 geom_errorbar(aes(ymin=proportion-stdev, ymax=proportion+stdev), width=.1) + 
  theme_bw()



# percent stacked barplots
date <- "2024-08-30"
data <- c()
for(i in 1:11) {
  new_data <- read.csv(paste0("data/", date, "/data_" , i, ".csv"))
  data <- rbind(data, new_data)
}
data <- cbind(data, dispersal = 0)

date <- "2024-08-30b"
for(i in 1:11) {
  new_data <- read.csv(paste0("data/", date, "/data_" , i, ".csv"))
  new_data <- cbind(new_data, dispersal = 0.5)
  data <- rbind(data, new_data)
}

date <- "2024-08-30c"
for(i in 1:11) {
  new_data <- read.csv(paste0("data/", date, "/data_" , i, ".csv"))
  new_data <- cbind(new_data, dispersal = 1)
  data <- rbind(data, new_data)
}

# make these percentages
data[4:6] <- data[4:6]/1000

reshaped_data <- data %>%
  group_by(dispersal, misclassification) %>%
  summarise(
    category = c("Euploid", "Mosaic Aneuploid", "Fully Aneuploid"),
    mean = c(mean(Euploid), mean(Mosaic.Aneuploid), mean(Fully.Aneuploid)),
    stdev = c(sd(Euploid), sd(Mosaic.Aneuploid), sd(Fully.Aneuploid))
  )

# percentages
# Horizontal percentage bar chart
ggplot(reshaped_data, aes(
  x = factor(misclassification),
  y = mean,
  fill = factor(
    category,
    levels = c("Fully Aneuploid", "Mosaic Aneuploid", "Euploid")
  )
)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(factor(dispersal, levels = c("0", "0.5", "1"))), scales = "fixed") +
  labs(x = "Misclassification Rate",
       y = "Proportion of Embryos",
       fill = "Embryo Type") +
  scale_y_continuous(expand = c(0, 0),
    sec.axis = sec_axis(
    ~ . ,
    name = "Dispersal",
    breaks = NULL,
    labels = NULL
  )) +
  scale_fill_viridis(discrete = TRUE) +
  theme_classic()

reshaped_data <- reshaped_data %>%
  mutate(new_mean = cumsum(mean))

# With error bars
ggplot(reshaped_data, aes(
  x = factor(misclassification),
  y = mean,
  fill = factor(
    category,
    levels = c("Fully Aneuploid", "Mosaic Aneuploid", "Euploid")
  )
)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(factor(dispersal, levels = c("0", "0.5", "1"))), scales = "fixed") +
  labs(x = "Misclassification Rate",
       y = "Proportion of Embryos",
       fill = "Embryo Type") +
  scale_y_continuous(expand = c(0, 0),
                     sec.axis = sec_axis(
                       ~ . ,
                       name = "Dispersal",
                       breaks = NULL,
                       labels = NULL
                     )) +
  geom_errorbar(aes(ymin = new_mean - stdev, ymax = new_mean + stdev),
                width = 0.2,
                color = "red") +
  scale_fill_viridis(discrete = TRUE) +
  theme_classic()

#### Figure S2 ##############

data1 <- read.csv("data/2024-08-16c/data.csv")
data2 <- read.csv("data/2024-08-16d/data.csv")
data3 <- read.csv("data/2024-08-16e/data.csv")
dispersal_ranges <- rbind(data1, data2, data3)

biopsy_types <- dispersal_ranges %>%
  group_by(dispersal) %>%
  summarise(
    category = c("Euploid", "Mosaic", "Aneuploid"),
    mean = c(mean(euploid), mean(mosaic), mean(aneuploid)),
            stdev = c(sd(euploid), sd(mosaic), sd(aneuploid))) %>%
  mutate(ypos = cumsum(mean)-0.05)

biopsy <- ggplot(biopsy_types, aes(
  x = factor(dispersal, levels = c(1, 0.5, 0)),
  y = mean,
  fill = factor(
    category,
    levels = c("Euploid", "Mosaic", "Aneuploid")
  )
)) +
  geom_bar(stat = "identity") +
  labs(x = "Dispersal",
       y = "Proportions of Biopsies",
       fill = "Biopsy Type",
       tag = "B") +
  geom_label(
    aes(y = ypos, label = sprintf("%.1f%%", mean*100)),
    color = "red",
    fill = "white",
    fontface = "bold",
    size = 4
  ) +
  scale_fill_viridis(discrete = TRUE)+
  scale_y_continuous(expand = c(0, 0),labels = scales::percent_format()) +
  theme_classic()

data <- data.frame(
  category = c("Euploid", "Mosaic", "Aneuploid"),
  value = c(0.388, 0.186, 0.426)
)

data <- data %>%
  mutate(ypos = cumsum(value) - 0.1)

# Plot the single stacked bar chart
ref <- ggplot(data, aes(x = 1, y = value, fill = factor(
  category,
  levels = c("Euploid", "Mosaic", "Aneuploid")
))) + 
  geom_bar(stat = "identity", width = 0.05) +
  labs(x = "", y = "Percentage", fill = "Category", tag = "A") + 
  ggtitle("Reference Proportions from Viotti et al. 2021")  +
  geom_label(
    aes(y = ypos, label = sprintf("%.1f%%", value*100)),
    color = "red",
    fill = "white",
    fontface = "bold",
    size = 4
  )   +
  scale_fill_viridis(discrete = TRUE) +
  theme_void()+
  theme(    axis.text = element_blank(),         # Hide text on both axes
            axis.ticks = element_blank(),        # Hide ticks on both axes
            axis.title = element_blank(),        # Hide axis titles
            panel.grid = element_blank(),
            legend.position = "none")
ref + biopsy