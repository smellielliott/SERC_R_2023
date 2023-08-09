# PX graphs
# 07/3/2023
# S. Elliott

# set directory
# select "session" -> "set working directory" -> "to source file location"

setwd("C:/Users/SGEll/OneDrive/Desktop/Fieldscope R")

# load libraries

library(tidyverse)
library(readxl)
library(writexl)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(dplyr)

# set up data

hc_data <- read.csv("./ProcessedData/My_SGE_07252023_averages.csv")

# filter and set as numeric

PX_data_filter <- hc_data %>%
  filter(station != c('RRDOCK')) %>%
  filter(station != c('PX_SIP_SGE')) %>%
  filter(station != c('PX_LM_SGE'))

PX_data_clean <- subset(PX_data_filter, PX_data_filter$turbidity_means < 85)

  

as.numeric(PX_data_clean$hydrocolor_means)
as.numeric(PX_data_clean$turbidity_means)

# average comparison graph

raw_graph <- ggplot(PX_data_clean, aes(y=turbidity_means, 
                                       x=hydrocolor_means, 
                                       colour= station)) + 
  geom_point(size = 2.5) + 
  labs(title="Hydrocolor 2.0 Data for Patuxent River", 
       y="Average Turbidity", x="Average Hydrocolor", 
       subtitle= "n = 6   n = 14   n = 2   n = 4  Station") +
  theme_classic() + ylim(0,80) +
  scale_color_manual(values = c("#6d9eeb", "#f6b26b", "#93c47d", "#8e7cc3"))

raw_graph # print graph to check

# adding regression line and R2 (GGplot)

raw_graph + geom_smooth(method="lm", se=FALSE, formula = y~x) + 
  stat_cor(label.y.npc = 0.8,
           aes(label = paste(..rr.label.., sep = "~`,`~"))) +
  stat_regline_equation()

# boxplot

ggboxplot(PX_data_clean, x = "station", y = "hydrocolor_means",
          color = "station", 
          add = c("mean_sd", "jitter"),
          order = c("PX_KL_SGE", "PX_HPD_SGE", "PX_LM_SGE", "PX_SIP_SGE")) + 
  scale_color_manual(values = c("#6d9eeb", "#f6b26b", "#93c47d", "#8e7cc3")) +
  labs(x= "Station", 
       y= "Average Hydrocolor Measurement", 
       title= "Station Comparisons for Patuxent River",
       subtitle= "n = 14   n = 7   n = 2   n = 4") + 
  scale_y_continuous(breaks = seq(0, 28, 2), limits=c(0, 28))

# save figure

ggsave("filtered_final_stationgraph_PX.png", 
       width = 20, height = 20, units = "cm")

# ANOVA and statistical testing

res.aov <- aov(hydrocolor_means ~ station, data = PX_data_clean)
summary(res.aov) # see summary of analysis
TukeyHSD(res.aov)

res.aov1 <- aov(hydrocolor_means ~ phone_type, data = PX_data_clean)
summary(res.aov1) # see summary of analysis
TukeyHSD(res.aov1)

aggregate(PX_data_clean$hydrocolor_means, 
          list(PX_data_clean$station), median)
aggregate(PX_data_clean$hydrocolor_means, 
          list(PX_data_clean$station), mean)

# two way ANOVA

res.aov2 <- aov(hydrocolor_means ~ phone_type + station, data = PX_data_clean)
summary(res.aov2)
TukeyHSD(res.aov2)

# to check in excel if equation is accurate

write_xlsx(PX_data_clean,"./ProcessedData/Cleaned_PX_data_validation.xlsx")


