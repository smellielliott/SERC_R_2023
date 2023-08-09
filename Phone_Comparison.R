# Comparisons data manipulation and graphing
# 7_14_2023
# S. Elliott

#set directory

setwd("C:/Users/SGEll/OneDrive/Desktop/Hydrocolor_Fieldscope_R")

# load libraries

library(tidyverse)
library(ggplot2)
library(writexl)
library(readxl)
library(lubridate)
library(dplyr)
library(ggpubr)

# download all files

sge_phone <- read.csv("./ProcessedData/My_SGE_07252023_averages.csv")
rpr_phone <- read.csv("./ProcessedData/My_RPR_averages.csv")

# set up data

direct_data <- rbind(sge_phone, rpr_phone)

# add/remove columns

direct_data_filtered <- direct_data %>% 
  filter(station != c('PX_HPD_SGE')) %>%
  filter(!is.na(phone_type))

direct_PXdata_filtered <- direct_data %>% 
  filter(station != c('RRDOCK')) %>%
  filter(!is.na(phone_type)) %>%
  filter(station != c('PX_SIP_SGE')) %>%
  filter(station != c('PX_LM_SGE'))

direct_data_clean <- slice(direct_data_filtered, -c(28:30, 21))

# create means and standard deviations

direct_data_clean$hydrocolor_means <- apply(direct_data_clean[,4:6],1,
                                       mean, na.rm=TRUE)

direct_data_clean$turbidity_means <- apply(direct_data_clean[,9:11],1,
                                      mean, na.rm=TRUE)

direct_data_clean$hydrocolor_std <- apply(direct_data_clean[,4:6],1,
                                     sd, na.rm=TRUE)

direct_data_clean$turbidity_std <- apply(direct_data_clean[,9:11],1,
                                    sd, na.rm=TRUE)

# for PX

direct_PXdata_filtered$hydrocolor_means <- apply(direct_PXdata_filtered[,4:6],1,
                                            mean, na.rm=TRUE)

direct_PXdata_filtered$turbidity_means <- apply(direct_PXdata_filtered[,9:11],1,
                                           mean, na.rm=TRUE)

direct_PXdata_filtered$hydrocolor_std <- apply(direct_PXdata_filtered[,4:6],1,
                                          sd, na.rm=TRUE)

direct_PXdata_filtered$turbidity_std <- apply(direct_PXdata_filtered[,9:11],1,
                                         sd, na.rm=TRUE)

# graphing

ggboxplot(direct_data_clean, x = "phone_type", y = "hydrocolor_means",
          color = "phone_type", 
          add = c("mean_sd", "jitter"), 
          order = c("Android", "Apple")) + 
  scale_color_manual(values = c("#6d9eeb", "#f6b26b")) +
  labs(x= "Phone Type", 
       y= "Average Hydrocolor Measurement", 
       title= "Direct Phone Comparisons for Hydrocolor 2.0",
       subtitle= "n = 13")

ggboxplot(direct_PXdata_clean, x = "station", y = "hydrocolor_means",
          color = "phone_type", 
          add = c("mean_sd", "jitter"),
          order = c("PX_KL_SGE", "PX_HPD_SGE", "PX_LM_SGE", "PX_SIP_SGE")) + 
  scale_color_manual(values = c("#6d9eeb", "#f6b26b", "#93c47d", "#8e7cc3")) +
  labs(x= "Station", 
       y= "Average Hydrocolor Measurement", 
       title= "Station Comparisons for Patuxent River",
       subtitle= "n = 8   n = 14   n = 2   n = 4") + 
  scale_y_continuous(breaks = seq(0, 28, 2), limits=c(0, 28))

raw_graph <- ggplot(direct_PXdata_filtered,
                    aes(x=turbidity_means,
                        y=hydrocolor_means, color = station)) + 
  geom_point(size = 2.5) + theme_classic() + 
  labs(title= "Hydrocolor 2.0 Data (March 2023 - present)",
       x= "Average Hydrocolor",
       y= "Average Turbidity",
       subtitle= "n = 79") + 
  scale_color_manual(values = c("#6d9eeb", "#f6b26b", "#93c47d", "#8e7cc3"))

raw_graph # save graph

ggsave("phone_directcomparison_07262023.png", width = 20, height = 20, units = "cm")


# ANOVA and statistical testing

phone.aov <- aov(hydrocolor_means ~ phone_type, data = direct_data_clean)
summary(phone.aov) # see summary of analysis
TukeyHSD(phone.aov)

station.aov <- aov(hydrocolor_means ~ station, data = direct_data_clean)
summary(station.aov) # see summary of analysis
TukeyHSD(station.aov)

# two way ANOVA

res.aov2 <- aov(hydrocolor_means ~ station + phone_type, data = direct_data_clean)
summary(res.aov2)
TukeyHSD(res.aov2)

aggregate(direct_data_clean$hydrocolor_means, 
          list(direct_data_clean$phone_type), median)
aggregate(direct_data_clean$hydrocolor_means, 
          list(direct_data_clean$phone_type), mean)
