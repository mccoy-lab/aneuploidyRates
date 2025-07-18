# This file loads the generated data of meiotic and mitotic rates and plots them
# using different visualization methods.

# To start with, load all the packages right below.

# All figures for the paper are under each specific section titles. 


## Relevant data info in the data folder ##

# Currently in use:

# 04-04c, d, e -- 3000 ABC_seq Lenormand data for Capalbo

# 04-08c, d, e -- 3000 ABC_seq Lenormand data for Munne 2017

# 04-16c, d, e -- 3000 ABC_seq Lenormand data for Walters-Sen

# 04-18c, d, e -- 3000 ABC_seq Lenormand data for Rodrigo

# 04-19c, d, e -- 3000 ABC_seq Lenormand data for Clarke

# 04-21 -- misdiagnosed rates applied in expected values, dispersal 0, 0.5, 1, 
# for Capalbo

# 04-22 -- generated embryos based on distributions in 04-21

# 07-18_results -- 3,000,000 embryos created based on 04-04 data with two 
# biopsies taken 


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
if(!require(knitr)){
  install.packages("knitr")
}
library(knitr)
if(!require(tidyr)){
  install.packages("tidyr")
}
library(tidyr)
if(!require(ggalluvial)){
  install.packages("ggalluvial")
}
library(ggalluvial)
if(!require(readr)){
  install.packages("readr")
}
library(readr)
#### Figure 3 #############################################################

data1 <- read.csv("data/2025-04-04c/data.csv")
data2 <- read.csv("data/2025-04-04d/data.csv")
data3 <- read.csv("data/2025-04-04e/data.csv")
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
  # geom_vline(
  #   data = max_estimates,
  #   aes(xintercept = MAP_Estimate),
  #   color = "red",
  #   linewidth = 0.75,
  #   linetype = "dashed"
  # ) +
  theme_bw()

# Save space
rm(dispersal_ranges, data_melt)

#### Figure 4 ##################################################

# Read prop.aneu data to create dispersal_ranges
data1 <- read.csv("data/2025-04-04c/full_data.csv")
data2 <- read.csv("data/2025-04-04d/full_data.csv")
data3 <- read.csv("data/2025-04-04e/full_data.csv")
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
    aes(y = after_stat(count) / total_count),
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
    linewidth = 0.5,
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

# establish embryo categoriess based on the proportion of aneuploid cells
biopsy_data <- dispersal_ranges %>%
  mutate(
    category = case_when(
      prop.aneu == 0 ~ "Euploid",
      prop.aneu > 0 & prop.aneu < 1 ~ "Mosaic Aneuploid",
      prop.aneu == 1 ~ "Fully Aneuploid"
    ),
    count = 1
  )

embryo_types <- biopsy_data %>%
  group_by(dispersal, category) %>%
  summarise(total_count = sum(count), .groups = "drop") 

embryo_types$category <- factor(embryo_types$category,
                                levels = c("Euploid", "Mosaic Aneuploid", "Fully Aneuploid"))

# calculate percentages of each embryo type at each dispersal level
embryo_percentages <- embryo_types %>%
  group_by(dispersal) %>%
  mutate(
    total_embryos = sum(total_count),
    percent = total_count / total_embryos * 100
  )%>% # for labeling the percent bars
  mutate(label_ypos = percent + 0.5) 


# calculate mean and standard deviations
embryo_sum <- embryo_percentages %>%
  group_by(dispersal, category) %>%
  summarize(mean = mean(percent)) 

# Check if Euploid at dispersal 0 exists
euploid_missing <- embryo_sum %>%
  filter(dispersal == 0, category == "Euploid") %>%
  nrow() == 0

# Add a filler for the missing row
if (euploid_missing) {
  embryo_sum <- embryo_sum %>%
    ungroup() %>%
    add_row(dispersal = 0, category = "Euploid", mean = 0)
}

embryo_sum <- embryo_sum %>%
  mutate(xpos = c(12, 40, 80)[match(category, c("Euploid", "Mosaic Aneuploid", "Fully Aneuploid"))]) %>%
  arrange(dispersal, category) %>%
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
  scale_fill_viridis(discrete = TRUE) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() + coord_flip()


# Plot the single stacked bar chart

data <- data.frame(
  category = c("Euploid", "Mosaic", "Aneuploid"),
  value = c(0.232, 0.187, 0.581)
)

data <- data %>%
  mutate(ypos = c(0.20, 0.50, 0.80))

ref <- ggplot(data, aes(x = 1, y = value, fill = factor(
  category,
  levels = c("Aneuploid", "Mosaic", "Euploid")
))) + 
  geom_bar(stat = "identity", width = 0.5) +
  labs(x = "", y = "Percentage", fill = "Biopsy Type") + 
  ggtitle("Reference Proportions from Capalbo et al. 2021")  +
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

# Save space
rm(dispersal_ranges)

#### Table S1 & S2 #########################################################
# For Capalbo

data1 <- read.csv("data/2025-04-04c/data.csv")
data2 <- read.csv("data/2025-04-04d/data.csv")
data3 <- read.csv("data/2025-04-04e/data.csv")
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

# Save space
rm(dispersal_ranges, data_melt)

#### Table 1  #########################################################
# Set up tables
dispersal_0_stats_sum <- c("", "", "Euploid", "Mosaic", "Aneuploid", 
    "Prob. Meio", "Prob. Mito", 
    "Euploid Embryo", "Mosaic Aneuploid", 
    "Fully Aneuploid")

dispersal_0.5_stats_sum <- c("", "", "Euploid", "Mosaic", "Aneuploid", 
                           "Prob. Meio", "Prob. Mito", 
                           "Euploid Embryo", "Mosaic Aneuploid", 
                           "Fully Aneuploid")

dispersal_1_stats_sum <- c("", "", "Euploid", "Mosaic", "Aneuploid", 
                           "Prob. Meio", "Prob. Mito", 
                           "Euploid Embryo", "Mosaic Aneuploid", 
                           "Fully Aneuploid")

# Capalbo ----
Cap1 <- read.csv("data/2025-04-04c/full_data.csv")
Cap2 <- read.csv("data/2025-04-04d/full_data.csv")
Cap3 <- read.csv("data/2025-04-04e/full_data.csv")
Cap_dispersal_ranges <- rbind(Cap1, Cap2, Cap3)

# Save space
rm(Cap1, Cap2, Cap3)

# extract embryo data from sampling posterior error rate parameters
Cap_error_rate_melt <- melt(
  Cap_dispersal_ranges,
  id.vars = c("dispersal"),
  measure.vars = c("prob.meio", "prob.mito")
)

Cap_max_estimates <- Cap_error_rate_melt %>%
  group_by(dispersal, variable) %>%
  summarise(MAP_Estimate = signif(map_estimate(value)[2], 2), .groups = "drop") %>%   
  pivot_wider(names_from = variable, values_from = MAP_Estimate) 


# extract biopsy data
Cap_biopsy_melt <- melt(
  Cap_dispersal_ranges,
  id.vars = c("dispersal"),
  measure.vars = c("euploid", "mosaic", "aneuploid")
)

# Compute the mean of biopsy data at each dispersal level
Cap_mean_estimates <- Cap_biopsy_melt %>%
  group_by(dispersal, variable) %>%
 summarise(Mean = signif(mean(value), 2), .groups = "drop")  %>%
  pivot_wider(names_from = variable, values_from = Mean)

# extract embryo data
Cap_biopsy_data <- Cap_dispersal_ranges %>%
  mutate(
    category = case_when(
      prop.aneu == 0 ~ "Euploid",
      prop.aneu > 0 & prop.aneu < 1 ~ "Mosaic Aneuploid",
      prop.aneu == 1 ~ "Fully Aneuploid"
    )
  )

Cap_proportions <- Cap_biopsy_data %>%
  group_by(dispersal, category) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(dispersal) %>%
  mutate(Proportion = signif(Count / sum(Count), 2))  %>%
  select(dispersal, category, Proportion) %>%
  pivot_wider(names_from = category, values_from = Proportion)

# Save space
rm(Cap_dispersal_ranges, Cap_error_rate_melt, Cap_biopsy_melt, Cap_biopsy_data)

# Add data to a table by dispersal
dispersal_0_stats_sum <- rbind(
  c("Capalbo", 0, 
    # biopsy parameters
    # should be approximately the same biopsy set across all
    Cap_mean_estimates %>% filter(dispersal == 0) %>% pull(euploid),
    Cap_mean_estimates %>% filter(dispersal == 0) %>% pull(mosaic),
    Cap_mean_estimates %>% filter(dispersal == 0) %>% pull(aneuploid),
    Cap_max_estimates %>% filter(dispersal == 0) %>% pull(prob.meio),
    Cap_max_estimates %>% filter(dispersal == 0) %>% pull(prob.mito),
    Cap_proportions %>% filter(dispersal == 0) %>% pull(Euploid),
    Cap_proportions %>% filter(dispersal == 0) %>% pull("Mosaic Aneuploid"),
    Cap_proportions %>% filter(dispersal == 0) %>% pull("Fully Aneuploid")
  )
)

dispersal_0.5_stats_sum <- rbind(
  c("Capalbo", 0.5, 
    # biopsy parameters
    # should be approximately the same biopsy set across all
    Cap_mean_estimates %>% filter(dispersal == 0.5) %>% pull(euploid),
    Cap_mean_estimates %>% filter(dispersal == 0.5) %>% pull(mosaic),
    Cap_mean_estimates %>% filter(dispersal == 0.5) %>% pull(aneuploid),
    Cap_max_estimates %>% filter(dispersal == 0.5) %>% pull(prob.meio),
    Cap_max_estimates %>% filter(dispersal == 0.5) %>% pull(prob.mito),
    Cap_proportions %>% filter(dispersal == 0.5) %>% pull(Euploid),
    Cap_proportions %>% filter(dispersal == 0.5) %>% pull("Mosaic Aneuploid"),
    Cap_proportions %>% filter(dispersal == 0.5) %>% pull("Fully Aneuploid")
  )
)

dispersal_1_stats_sum <- rbind(
  c("Capalbo", 1, 
    # biopsy parameters
    # should be approximately the same biopsy set across all
    Cap_mean_estimates %>% filter(dispersal == 1) %>% pull(euploid),
    Cap_mean_estimates %>% filter(dispersal == 1) %>% pull(mosaic),
    Cap_mean_estimates %>% filter(dispersal == 1) %>% pull(aneuploid),
    Cap_max_estimates %>% filter(dispersal == 1) %>% pull(prob.meio),
    Cap_max_estimates %>% filter(dispersal == 1) %>% pull(prob.mito),
    Cap_proportions %>% filter(dispersal == 1) %>% pull(Euploid),
    Cap_proportions %>% filter(dispersal == 1) %>% pull("Mosaic Aneuploid"),
    Cap_proportions %>% filter(dispersal == 1) %>% pull("Fully Aneuploid")
  )
)



# Clarke ----- 
Clarke1 <- read.csv("data/2025-04-19c/full_data.csv")
Clarke2 <- read.csv("data/2025-04-19d/full_data.csv")
Clarke3 <- read.csv("data/2025-04-19e/full_data.csv")
Clarke_dispersal_ranges <- rbind(Clarke1, Clarke2, Clarke3)

# Save space
rm(Clarke1, Clarke2, Clarke3)

# extract embryo data from sampling posterior error rate parameters
Clarke_error_rate_melt <- melt(
  Clarke_dispersal_ranges,
  id.vars = c("dispersal"),
  measure.vars = c("prob.meio", "prob.mito")
)

Clarke_max_estimates <- Clarke_error_rate_melt %>%
  group_by(dispersal, variable) %>%
      summarise(MAP_Estimate = signif(map_estimate(value)[2], 2), .groups = "drop") %>%
  pivot_wider(names_from = variable, values_from = MAP_Estimate) 


# extract biopsy data
Clarke_mean_estimates <- data.frame(
  dispersal = c(0, 0.5, 1),
  euploid = rep(0.49, 3),
  mosaic = rep(0.18, 3),
  aneuploid = rep(0.33, 3)
)

# extract embryo data
Clarke_biopsy_data <- Clarke_dispersal_ranges %>%
  mutate(
    category = case_when(
      prop.aneu == 0 ~ "Euploid",
      prop.aneu > 0 & prop.aneu < 1 ~ "Mosaic Aneuploid",
      prop.aneu == 1 ~ "Fully Aneuploid"
    )
  )

Clarke_proportions <- Clarke_biopsy_data %>%
  group_by(dispersal, category) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(dispersal) %>%
  mutate(Proportion = signif(Count / sum(Count), 2))  %>%
  select(dispersal, category, Proportion) %>%
  pivot_wider(names_from = category, values_from = Proportion)


# Save space
rm(Clarke_dispersal_ranges, Clarke_error_rate_melt, Clarke_biopsy_melt, Clarke_biopsy_data)

# Add data to a table by dispersal
dispersal_0_stats_sum <- rbind(
  dispersal_0_stats_sum,
  c("Clarke", 0, 
    # biopsy parameters
    # should be approximately the same biopsy set across all
    Clarke_mean_estimates %>% filter(dispersal == 0) %>% pull(euploid),
    Clarke_mean_estimates %>% filter(dispersal == 0) %>% pull(mosaic),
    Clarke_mean_estimates %>% filter(dispersal == 0) %>% pull(aneuploid),
    Clarke_max_estimates %>% filter(dispersal == 0) %>% pull(prob.meio),
    Clarke_max_estimates %>% filter(dispersal == 0) %>% pull(prob.mito),
    Clarke_proportions %>% filter(dispersal == 0) %>% pull(Euploid),
    Clarke_proportions %>% filter(dispersal == 0) %>% pull("Mosaic Aneuploid"),
    Clarke_proportions %>% filter(dispersal == 0) %>% pull("Fully Aneuploid")
  )
)

dispersal_0.5_stats_sum <- rbind(
  dispersal_0.5_stats_sum,
  c("Clarke", 0.5, 
    # biopsy parameters
    # should be approximately the same biopsy set across all
    Clarke_mean_estimates %>% filter(dispersal == 0.5) %>% pull(euploid),
    Clarke_mean_estimates %>% filter(dispersal == 0.5) %>% pull(mosaic),
    Clarke_mean_estimates %>% filter(dispersal == 0.5) %>% pull(aneuploid),
    Clarke_max_estimates %>% filter(dispersal == 0.5) %>% pull(prob.meio),
    Clarke_max_estimates %>% filter(dispersal == 0.5) %>% pull(prob.mito),
    Clarke_proportions %>% filter(dispersal == 0.5) %>% pull(Euploid),
    Clarke_proportions %>% filter(dispersal == 0.5) %>% pull("Mosaic Aneuploid"),
    Clarke_proportions %>% filter(dispersal == 0.5) %>% pull("Fully Aneuploid")
  )
)

dispersal_1_stats_sum <- rbind(
  dispersal_1_stats_sum,
  c("Clarke", 1, 
    # biopsy parameters
    # should be approximately the same biopsy set across all
    Clarke_mean_estimates %>% filter(dispersal == 1) %>% pull(euploid),
    Clarke_mean_estimates %>% filter(dispersal == 1) %>% pull(mosaic),
    Clarke_mean_estimates %>% filter(dispersal == 1) %>% pull(aneuploid),
    Clarke_max_estimates %>% filter(dispersal == 1) %>% pull(prob.meio),
    Clarke_max_estimates %>% filter(dispersal == 1) %>% pull(prob.mito),
    Clarke_proportions %>% filter(dispersal == 1) %>% pull(Euploid),
    Clarke_proportions %>% filter(dispersal == 1) %>% pull("Mosaic Aneuploid"),
    Clarke_proportions %>% filter(dispersal == 1) %>% pull("Fully Aneuploid")
  )
)




# Munne----
Mun1 <- read.csv("data/2025-04-08c/full_data.csv")
Mun2 <- read.csv("data/2025-04-08d/full_data.csv")
Mun3 <- read.csv("data/2025-04-08e/full_data.csv")
Mun_dispersal_ranges <- rbind(Mun1, Mun2, Mun3)

# Save space
rm(Mun1, Mun2, Mun3)

# extract embryo data from sampling posterior error rate parameters
Mun_error_rate_melt <- melt(
  Mun_dispersal_ranges,
  id.vars = c("dispersal"),
  measure.vars = c("prob.meio", "prob.mito")
)

Mun_max_estimates <- Mun_error_rate_melt %>%
  group_by(dispersal, variable) %>%
      summarise(MAP_Estimate = signif(map_estimate(value)[2], 2), .groups = "drop") %>%
  pivot_wider(names_from = variable, values_from = MAP_Estimate) 


Mun_mean_estimates <- data.frame(
  dispersal = c(0, 0.5, 1),
  euploid = rep(0.53, 3),
  mosaic = rep(0.15, 3),
  aneuploid = rep(0.32, 3)
)

# extract embryo data
Mun_biopsy_data <- Mun_dispersal_ranges %>%
  mutate(
    category = case_when(
      prop.aneu == 0 ~ "Euploid",
      prop.aneu > 0 & prop.aneu < 1 ~ "Mosaic Aneuploid",
      prop.aneu == 1 ~ "Fully Aneuploid"
    )
  )

Mun_proportions <- Mun_biopsy_data %>%
  group_by(dispersal, category) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(dispersal) %>%
  mutate(Proportion = signif(Count / sum(Count), 2))  %>%
  select(dispersal, category, Proportion) %>%
  pivot_wider(names_from = category, values_from = Proportion)

# Save space
rm(Mun_dispersal_ranges, Mun_error_rate_melt, Mun_biopsy_melt, Mun_biopsy_data)

# Add data to a table by dispersal
dispersal_0_stats_sum <- rbind(
  dispersal_0_stats_sum,
  c("Munne", 0, 
    # biopsy parameters
    # should be approximately the same biopsy set across all
    Mun_mean_estimates %>% filter(dispersal == 0) %>% pull(euploid),
    Mun_mean_estimates %>% filter(dispersal == 0) %>% pull(mosaic),
    Mun_mean_estimates %>% filter(dispersal == 0) %>% pull(aneuploid),
    Mun_max_estimates %>% filter(dispersal == 0) %>% pull(prob.meio),
    Mun_max_estimates %>% filter(dispersal == 0) %>% pull(prob.mito),
    Mun_proportions %>% filter(dispersal == 0) %>% pull(Euploid),
    Mun_proportions %>% filter(dispersal == 0) %>% pull("Mosaic Aneuploid"),
    Mun_proportions %>% filter(dispersal == 0) %>% pull("Fully Aneuploid")
  )
)

dispersal_0.5_stats_sum <- rbind(
  dispersal_0.5_stats_sum,
  c("Munne", 0.5, 
    # biopsy parameters
    # should be approximately the same biopsy set across all
    Mun_mean_estimates %>% filter(dispersal == 0.5) %>% pull(euploid),
    Mun_mean_estimates %>% filter(dispersal == 0.5) %>% pull(mosaic),
    Mun_mean_estimates %>% filter(dispersal == 0.5) %>% pull(aneuploid),
    Mun_max_estimates %>% filter(dispersal == 0.5) %>% pull(prob.meio),
    Mun_max_estimates %>% filter(dispersal == 0.5) %>% pull(prob.mito),
    Mun_proportions %>% filter(dispersal == 0.5) %>% pull(Euploid),
    Mun_proportions %>% filter(dispersal == 0.5) %>% pull("Mosaic Aneuploid"),
    Mun_proportions %>% filter(dispersal == 0.5) %>% pull("Fully Aneuploid")
  )
)

dispersal_1_stats_sum <- rbind(
  dispersal_1_stats_sum,
  c("Munne", 1, 
    # biopsy parameters
    # should be approximately the same biopsy set across all
    Mun_mean_estimates %>% filter(dispersal == 1) %>% pull(euploid),
    Mun_mean_estimates %>% filter(dispersal == 1) %>% pull(mosaic),
    Mun_mean_estimates %>% filter(dispersal == 1) %>% pull(aneuploid),
    Mun_max_estimates %>% filter(dispersal == 1) %>% pull(prob.meio),
    Mun_max_estimates %>% filter(dispersal == 1) %>% pull(prob.mito),
    Mun_proportions %>% filter(dispersal == 1) %>% pull(Euploid),
    Mun_proportions %>% filter(dispersal == 1) %>% pull("Mosaic Aneuploid"),
    Mun_proportions %>% filter(dispersal == 1) %>% pull("Fully Aneuploid")
  )
)


# Rodrigo----
Rodrigo1 <- read.csv("data/2025-04-18c/full_data.csv")
Rodrigo2 <- read.csv("data/2025-04-18d/full_data.csv")
Rodrigo3 <- read.csv("data/2025-04-18e/full_data.csv")
Rodrigo_dispersal_ranges <- rbind(Rodrigo1, Rodrigo2, Rodrigo3)

# Save space
rm(Rodrigo1, Rodrigo2, Rodrigo3)

# extract embryo data from sampling posterior error rate parameters
Rodrigo_error_rate_melt <- melt(
  Rodrigo_dispersal_ranges,
  id.vars = c("dispersal"),
  measure.vars = c("prob.meio", "prob.mito")
)

Rodrigo_max_estimates <- Rodrigo_error_rate_melt %>%
  group_by(dispersal, variable) %>%
      summarise(MAP_Estimate = signif(map_estimate(value)[2], 2), .groups = "drop") %>%
  pivot_wider(names_from = variable, values_from = MAP_Estimate) 


# extract biopsy data
Rodrigo_mean_estimates <- data.frame(
  dispersal = c(0, 0.5, 1),
  euploid = rep(0.51, 3),
  mosaic = rep(0.062, 3),
  aneuploid = rep(0.43, 3)
)

# extract embryo data
Rodrigo_biopsy_data <- Rodrigo_dispersal_ranges %>%
  mutate(
    category = case_when(
      prop.aneu == 0 ~ "Euploid",
      prop.aneu > 0 & prop.aneu < 1 ~ "Mosaic Aneuploid",
      prop.aneu == 1 ~ "Fully Aneuploid"
    )
  )

Rodrigo_proportions <- Rodrigo_biopsy_data %>%
  group_by(dispersal, category) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(dispersal) %>%
  mutate(Proportion = signif(Count / sum(Count), 2))  %>%
  select(dispersal, category, Proportion) %>%
  pivot_wider(names_from = category, values_from = Proportion)

# Save space
rm(Rodrigo_dispersal_ranges, Rodrigo_error_rate_melt, Rodrigo_biopsy_melt, Rodrigo_biopsy_data)

# Add data to a table by dispersal
dispersal_0_stats_sum <- rbind(
  dispersal_0_stats_sum,
  c("Rodrigo", 0, 
    # biopsy parameters
    # should be approximately the same biopsy set across all
    Rodrigo_mean_estimates %>% filter(dispersal == 0) %>% pull(euploid),
    Rodrigo_mean_estimates %>% filter(dispersal == 0) %>% pull(mosaic),
    Rodrigo_mean_estimates %>% filter(dispersal == 0) %>% pull(aneuploid),
    Rodrigo_max_estimates %>% filter(dispersal == 0) %>% pull(prob.meio),
    Rodrigo_max_estimates %>% filter(dispersal == 0) %>% pull(prob.mito),
    Rodrigo_proportions %>% filter(dispersal == 0) %>% pull(Euploid),
    Rodrigo_proportions %>% filter(dispersal == 0) %>% pull("Mosaic Aneuploid"),
    Rodrigo_proportions %>% filter(dispersal == 0) %>% pull("Fully Aneuploid")
  )
)

dispersal_0.5_stats_sum <- rbind(
  dispersal_0.5_stats_sum,
  c("Rodrigo", 0.5, 
    # biopsy parameters
    # should be approximately the same biopsy set across all
    Rodrigo_mean_estimates %>% filter(dispersal == 0.5) %>% pull(euploid),
    Rodrigo_mean_estimates %>% filter(dispersal == 0.5) %>% pull(mosaic),
    Rodrigo_mean_estimates %>% filter(dispersal == 0.5) %>% pull(aneuploid),
    Rodrigo_max_estimates %>% filter(dispersal == 0.5) %>% pull(prob.meio),
    Rodrigo_max_estimates %>% filter(dispersal == 0.5) %>% pull(prob.mito),
    Rodrigo_proportions %>% filter(dispersal == 0.5) %>% pull(Euploid),
    Rodrigo_proportions %>% filter(dispersal == 0.5) %>% pull("Mosaic Aneuploid"),
    Rodrigo_proportions %>% filter(dispersal == 0.5) %>% pull("Fully Aneuploid")
  )
)

dispersal_1_stats_sum <- rbind(
  dispersal_1_stats_sum,
  c("Rodrigo", 1, 
    # biopsy parameters
    # should be approximately the same biopsy set across all
    Rodrigo_mean_estimates %>% filter(dispersal == 1) %>% pull(euploid),
    Rodrigo_mean_estimates %>% filter(dispersal == 1) %>% pull(mosaic),
    Rodrigo_mean_estimates %>% filter(dispersal == 1) %>% pull(aneuploid),
    Rodrigo_max_estimates %>% filter(dispersal == 1) %>% pull(prob.meio),
    Rodrigo_max_estimates %>% filter(dispersal == 1) %>% pull(prob.mito),
    Rodrigo_proportions %>% filter(dispersal == 1) %>% pull(Euploid),
    Rodrigo_proportions %>% filter(dispersal == 1) %>% pull("Mosaic Aneuploid"),
    Rodrigo_proportions %>% filter(dispersal == 1) %>% pull("Fully Aneuploid")
  )
)



# Print table
kable(dispersal_0_stats_sum, format = "markdown", col.names = c(
  "Data set", "Dispersal", "", "Published Biopsy Data", "", "Inferred Error Rates",
  "", "", "Inferred Embryo Types",""
))

kable(dispersal_0.5_stats_sum, format = "markdown", col.names = c(
  "Data set", "Dispersal", "", "Published Biopsy Data", "", "Inferred Error Rates",
  "", "", "Inferred Embryo Types",""
))

kable(dispersal_1_stats_sum, format = "markdown", col.names = c(
  "Data set", "Dispersal", "", "Published Biopsy Data", "", "Inferred Error Rates",
  "", "", "Inferred Embryo Types",""
))

#### Figure 2 ###################################
# import dispersal_ranges
data1 <- read.csv("data/2025-04-04c/data.csv")
data2 <- read.csv("data/2025-04-04d/data.csv")
data3 <- read.csv("data/2025-04-04e/data.csv")
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

# Save space
rm(dispersal_ranges)

#### Figure 5 ##############
# percent stacked barplots
date <- "2025-04-22"
data <- c()
for(i in 1:11) {
  new_data <- read.csv(paste0("data/", date, "/data_" , i, ".csv"))
  data <- rbind(data, new_data)
}
data <- cbind(data, dispersal = 0)

date <- "2025-04-22b"
for(i in 1:11) {
  new_data <- read.csv(paste0("data/", date, "/data_" , i, ".csv"))
  new_data <- cbind(new_data, dispersal = 0.5)
  data <- rbind(data, new_data)
}

date <- "2025-04-22c"
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


# Proper summarization and reshaping
reshaped_data <- data %>%
  group_by(dispersal, misclassification) %>%
  summarise(
    Euploid_mean = mean(Euploid),
    Mosaic_mean = mean(Mosaic.Aneuploid),
    Fully_mean = mean(Fully.Aneuploid),
    Euploid_sd = sd(Euploid),
    Mosaic_sd = sd(Mosaic.Aneuploid),
    Fully_sd = sd(Fully.Aneuploid),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -c(dispersal, misclassification),
    names_to = c("category", ".value"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  mutate(category = factor(category, levels = c("Fully", "Mosaic", "Euploid"))) %>%
  group_by(dispersal, misclassification) %>%
  arrange(category) %>%
  mutate(
    cumulative_mean = cumsum(mean),
    cumulative_lower = cumulative_mean - sd,
    cumulative_upper = cumulative_mean + sd
  )

# Plotting with correctly positioned error bars
ggplot(reshaped_data, aes(
  x = factor(misclassification),
  y = mean,
  fill = category
)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = cumulative_lower, ymax = cumulative_upper),
                width = 0.2, color = "red") +
  facet_grid(rows = vars(factor(dispersal, levels = c("0", "0.5", "1"))),
             scales = "fixed") +
  labs(x = "Misclassification Rate",
       y = "Proportion of Embryos",
       fill = "Embryo Type") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis(discrete = TRUE,
                     labels = c("Fully Aneuploid", "Mosaic Aneuploid", "Euploid")) +
  theme_classic()

# Check numeric values for error bars clearly
error_bar_values <- reshaped_data %>%
  select(dispersal, misclassification, category, mean, sd,
         cumulative_mean, cumulative_lower, cumulative_upper)

print(error_bar_values)
#### Figure S2 ##############

data1 <- read.csv("data/2025-04-04c/data.csv")
data2 <- read.csv("data/2025-04-04d/data.csv")
data3 <- read.csv("data/2025-04-04e/data.csv")
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
  value = c(0.232, 0.181, 0.587)
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
  ggtitle("Reference Proportions from Capalbo et al. 2021")  +
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

# Save space
rm(dispersal_ranges)


#### Figure S3 ###########################
# Compile results
results <- list.files("2025-07-18_results", full.names = TRUE, pattern = "*.csv")
combined <- bind_rows(lapply(results, read_csv))

# if those columns exist
combined <- combined %>% select(-embryo)

# Save this file
write.csv(combined, "embryo_biopsy_complete_data.csv")

df <- combined %>%
  group_by(dispersal, embryo_type, first_biopsy_type, second_biopsy_type) %>%
  summarise(total = n(), .groups = "drop") %>%
  arrange(desc(total))



##### Summarize total count of biopsy types #####
biopsy_total_count <- df %>%
  group_by(dispersal, first_biopsy_type) %>%
  summarise(count = sum(total), .groups = "drop") %>%
  mutate(percent = round((count / 1e6) * 100, 1)) 

biopsy_totals_pivot <- biopsy_total_count %>%
  pivot_wider(
    names_from = dispersal,
    values_from = c(count, percent),
    names_glue = "{.value}_dispersal_{dispersal}"
  ) %>%
  mutate(first_biopsy_type = factor(first_biopsy_type, levels = c("Euploid", "Mosaic", "Aneuploid")))%>%
  arrange(first_biopsy_type)
kable(biopsy_totals_pivot, format = "markdown")

# Summarize total counts by embryo → biopsy type
biopsy_summary <- df %>%
  group_by(dispersal, embryo_type, first_biopsy_type, second_biopsy_type) %>%
  summarise(flow = sum(total), .groups = "drop") %>%
  mutate(
    embryo_type = factor(embryo_type, levels = c("Euploid", "Mosaic Aneuploid", "Fully Aneuploid")),
    first_biopsy_type = factor(first_biopsy_type, levels = c("Euploid", "Mosaic", "Aneuploid")),
    second_biopsy_type = factor(second_biopsy_type, levels = c("Euploid", "Mosaic", "Aneuploid"))
  ) %>%
  group_by(dispersal, first_biopsy_type) %>%
  mutate(percentage = round(flow / sum(flow) * 100, 1)) %>%
  ungroup() %>%
  arrange(dispersal, embryo_type, first_biopsy_type, second_biopsy_type)

# Print in Markdown
kable(biopsy_summary, format = "markdown")

# Compress the data 
biopsy_summary_pivot <- biopsy_summary %>%
  pivot_wider(
    names_from = dispersal,
    values_from = c(flow, percentage),
    names_glue = "Dispersal {dispersal} {.value}"
  )
# Print in Markdown
kable(biopsy_summary_pivot, format = "markdown")

##### aneupoid biopsy from mosaic embryos ####
result <- biopsy_summary %>%
  filter(first_biopsy_type == "Aneuploid") %>%
  mutate(is_mosaic = embryo_type == "Mosaic Aneuploid") %>%
  group_by(dispersal) %>%
  summarise(
    total_aneuploid_biopsy = sum(flow),
    from_mosaic_embryo = sum(flow[is_mosaic]),
    percent = round(100 * from_mosaic_embryo / total_aneuploid_biopsy, 1),
    .groups = "drop"
  )

# Print in Markdown
kable(result, format = "markdown")

##### euploid biopsy from mosaic embryos ####
result <- biopsy_summary %>%
  filter(first_biopsy_type == "Euploid") %>%
  mutate(is_mosaic = embryo_type == "Mosaic Aneuploid") %>%
  group_by(dispersal) %>%
  summarise(
    total_euploid_biopsy = sum(flow),
    from_mosaic_embryo = sum(flow[is_mosaic]),
    percent = round(100 * from_mosaic_embryo / total_euploid_biopsy, 1),
    .groups = "drop"
  )

# Print in Markdown
kable(result, format = "markdown")


##### First to second biopsy ######
# Arrange biopsy types by order
plot_df <- biopsy_summary %>%
  mutate(
    first_biopsy_type = factor(first_biopsy_type, levels = c("Euploid", "Mosaic", "Aneuploid")),
    second_biopsy_type = factor(second_biopsy_type, levels = c("Euploid", "Mosaic", "Aneuploid")),
    embryo_type = factor(embryo_type, levels = c("Euploid", "Mosaic Aneuploid", "Fully Aneuploid"))
  )

# Sankey plot
ggplot(plot_df,
       aes(
         axis1 = first_biopsy_type,
         axis2 = second_biopsy_type,
         y = flow,
         fill = embryo_type)) +
  geom_alluvium(width = 1/12, alpha = 0.8) +
  geom_stratum(width = 1/6, fill = "gray80", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3, color = "black") +
  facet_wrap(~ dispersal, labeller = label_both, nrow = 1,  scales = "free_y") +
  scale_x_discrete(limits = c("First Biopsy", "Second Biopsy"), expand = c(.1, .1)) +
  labs(title = "Biopsy Pair Type Flows by Embryo Dispersal Level",
       y = "Embryo Count",
       fill = "Embryo Type") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) 

## Reference Plots ##############

##### Biopsy Summary ##############

data1 <- read.csv("data/2025-04-04c/data.csv")
data2 <- read.csv("data/2025-04-04d/data.csv")
data3 <- read.csv("data/2025-04-04e/data.csv")
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
  value = c(0.23, 0.19, 0.58)
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

##### Check for biopsy summary compared to initial summaries #####
# Compile results
results <- list.files("2025-07-18_results", full.names = TRUE, pattern = "*.csv")
combined <- bind_rows(lapply(results, read_csv))

# if those columns exist
combined <- combined %>% select(-embryo)
# first biopsy
tally_by_meio_mito <- combined %>%
  group_by(dispersal, prob.meio, prob.mito, , euploid, mosaic, aneuploid, first_biopsy_type) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(dispersal, prob.meio, prob.mito, euploid, mosaic, aneuploid) %>%
  mutate(prop = count / sum(count)) %>%
  select(-count) %>%
  pivot_wider(names_from = first_biopsy_type, values_from = prop, values_fill = 0) %>%
  ungroup() %>%
  relocate(Mosaic, .before = Aneuploid) %>%
  relocate(Euploid, .before = Mosaic)

# second biopsy
tally_by_meio_mito_2 <- combined %>%
  group_by(dispersal, prob.meio, prob.mito, , euploid, mosaic, aneuploid, second_biopsy_type) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(dispersal, prob.meio, prob.mito, euploid, mosaic, aneuploid) %>%
  mutate(prop = count / sum(count)) %>%
  select(-count) %>%
  pivot_wider(names_from = second_biopsy_type, values_from = prop, values_fill = 0) %>%
  ungroup() %>%
  relocate(Mosaic, .before = Aneuploid) %>%
  relocate(Euploid, .before = Mosaic)
