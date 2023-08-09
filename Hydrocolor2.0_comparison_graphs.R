# Graphing for Hydrocolor 2.0 (April 2022-present)
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

hc_data <- read_csv("./ProcessedData/All_07072023_averages.csv") # for .CSV

hc_data <- read_xlsx("./ProcessedData/All_averages_07042023.xlsx") # for .xlsx

# only include HC2.0 data and remove PX data

hc2_data <- subset(hc_data, date > '2023-03-21')

hc2_data_filtered <- hc2_data %>%             # this one for any others
  filter(station != c('PX_KL_SGE')) %>%
  filter(station != c('PX_HPD_SGE')) %>%
  filter(station != c('HB_1')) %>%
  filter(station != c('HB_2')) %>%
  filter(station != c('HB_3'))


hc2_data_filtered <- hc2_data %>%             # this one for publication
  filter(station != c('PX_KL_SGE')) %>%
  filter(station != c('PX_HPD_SGE')) %>%
  filter(station != c('PX_SIP_SGE')) %>%
  filter(station != c('PX_LM_SGE')) %>%
  filter(station != c('HB_1')) %>%
  filter(station != c('HB_2')) %>%
  filter(station != c('HB_3')) %>%
  filter(!is.na(phone_type))
  
hc2_data_sliced <- slice(hc2_data_filtered, -c(272)) # remove any issues
# specific issue here: RR_CadleCreek_JM, 2023-04-12, 09:58:00, 14


# only include data that has both hydrocolor AND benchtop turbidity

hc2_data_clean <- hc2_data_sliced[!is.na(hc2_data_sliced$hydrocolor_means)
                                  & !is.na(hc2_data_sliced$turbidity_means),]

# set factors correctly as numeric

as.numeric(hc2_data_clean$hydrocolor_means)
as.numeric(hc2_data_clean$turbidity_means)

# average comparison graph

raw_graph <- ggplot(hc2_data_clean,
                    aes(x=hydrocolor_means,
                        y=turbidity_means)) + 
  geom_point(size = 2.5, colour = "#6d9eeb") + 
  ylim(0,25) + xlim(0,25) + theme_classic() + 
  labs(title= "Hydrocolor 2.0 Data (March 2023 - present)",
                       x= "Average Hydrocolor",
                       y= "Average Turbidity",
                       subtitle= "n = 79") +
  scale_color_manual(values = c("#6d9eeb",
                                "#f6b26b"))

raw_graph_phone <- ggplot(hc2_data_clean,
                     aes(x=hydrocolor_means,
                         y=turbidity_means,
                         colour= phone_type)) + 
  geom_point(size = 2.5) + 
  ylim(0,25) + xlim(0,25) + theme_classic() + 
  labs(title= "Hydrocolor 2.0 Data (March 2023 - present)",
       x= "Average Hydrocolor",
       y= "Average Turbidity",
       subtitle= "n = 79") +
  scale_color_manual(values = c("#6d9eeb",
                                "#f6b26b"))

raw_graph # print graph to check

# adding regression line and R2 (for basic GGplot)

raw_graph + geom_smooth(method="lm", se=FALSE, color = "black", formula = y~x) + 
  stat_cor(label.y = 25,
           aes(label = paste(after_stat(rr.label), sep = "~`,`~"))) +
  stat_regline_equation(label.y = 23)

# adding regression line and R2 (phone comparison GGplot)

raw_graph_phone + geom_smooth(method="lm", se=FALSE, formula = y~x) + 
  stat_cor(label.y.npc = 0.85, aes(label = paste(after_stat(rr.label), 
                                                 sep = "~`,`~")),) +
                                              stat_regline_equation()

# save figure

ggsave("HC2_comparison_graph.png")

ggsave("HC2_all_07252023.png", width = 20, height = 20, units = "cm")

# ANOVA and statistical testing

res.aov <- aov(hydrocolor_means ~ phone_type, data = hc2_data_clean)
summary(res.aov) # see summary of analysis
TukeyHSD(res.aov)

# two way ANOVA

res.aov2 <- aov(hydrocolor_means ~ phone_type + station, data = hc2_data_clean)
summary(res.aov2)
TukeyHSD(res.aov2)

aggregate(hc2_data_clean$hydrocolor_means, 
          list(hc2_data_clean$phone_type), median)

# export excel

write_xlsx(hc2_data_clean,"./ProcessedData/Cleaned_HC2_data_validation.xlsx")


