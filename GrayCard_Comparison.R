# Comparisons data manipulation and graphing
# 7_14_2023
# S. Elliott

#set directory

setwd("C:/Users/SGEll/OneDrive/Desktop/Fieldscope R")

# load libraries

library(tidyverse)
library(readxl)
library(writexl)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(dplyr)

# download all files

pjn_gc <- read_xlsx("./RawData/GrayCard_Comparison_PJN.xlsx")
rpr_gc <- read_xlsx("./RawData/GrayCard_Comparison_RR.xlsx")
sge_gc <- read_xlsx("./RawData/GrayCard_Comparison_SGE.xlsx")

# set up data

all_rawdata_GC <- rbind(pjn_gc, sge_gc, rpr_gc)

# set as numeric

all_rawdata_GC[, 7:18] <- sapply(all_rawdata_GC[, 7:18], as.factor)
all_rawdata_GC[, 7:18] <- sapply(all_rawdata_GC[, 7:18], as.numeric)
  
# add/remove columns

all_gc_filter <- all_rawdata_GC %>% 
  filter(stationName != c('PX_HPD_SGE'))

all_gc_cut <- subset(all_gc_filter, all_gc_filter$hydrocolor_turbidity3 < 60)

all_gc_select <- select(all_gc_cut, -c(3, 10:15, 20))

all_gc_clean <- slice(all_gc_select, -c(29, 25))

# create means and standard deviations

all_gc_clean$hydrocolor_means <- apply(all_gc_clean[,6:8],1,
                                           mean, na.rm=TRUE)

all_gc_clean$turbidity_means <- apply(all_gc_clean[,9:11],1,
                                          mean, na.rm=TRUE)

all_gc_clean$hydrocolor_std <- apply(all_gc_clean[,6:8],1,
                                       sd, na.rm=TRUE)

all_gc_clean$turbidity_std <- apply(all_gc_clean[,9:11],1,
                                      sd, na.rm=TRUE)

# filter data into subgroups
  
# graphing

# average comparison graph

ggplot(all_gc_clean, aes(x=turbidity_means, y=hydrocolor_means)) + 
  geom_point(aes(colour = cardType), size = 2.5) + 
  ggtitle("Turbidity vs HydroColor by Card Type") +
  ylab("Average Hydrocolor") + ylim(0,20) +
  xlab("Average Turbidity") + xlim(0,25) +
  theme_classic() + geom_smooth(method="lm", se=FALSE, 
                                color = "black", formula = y~x) + 
  stat_cor(label.y = 20,
           aes(label = paste(after_stat(rr.label), sep = "~`,`~"))) +
  stat_regline_equation(label.y = 18) +
  annotate("text", x = 0.75, y = 16, label = "n = 12")

# col chart with std

ggplot(all_gc_clean, aes(x=stationName, y=hydrocolor_means, fill= cardType)) +
  geom_col(position = "dodge") + theme_classic() +
  theme(legend.position="none") +
  geom_errorbar(aes(ymin=hydrocolor_means-hydrocolor_std,
                    ymax=hydrocolor_means+hydrocolor_std),
                width=.2, position=position_dodge(.9))

# box and whisker by station

ggboxplot(all_gc_clean, x = "stationName", y = "hydrocolor_means",
          color = "cardType", palette = c("#6d9eeb",
                                          "#f6b26b")) + 
  labs(title= "Reflectance Card Comparisons for Hydrocolor 2.0",
       x= "Observer",
       y= "Hydrocolor Measurement Mean", 
       subtitle= "n = 36") + ylim(0,20)

# points with means and std error bars

ggline(all_gc_clean, x = "cardType", y = "hydrocolor_means", color = "cardType", 
       add = c("mean_sd", "jitter"), 
       order = c("VGC", "PGC"),
       ylab = "Hydrocolor Measurement Mean", xlab = "Card Type") 

# boxplot w jitter

ggboxplot(all_gc_clean, x = "stationName", y = "hydrocolor_means", color = "cardType", 
       add = c("mean_sd", "jitter"), 
       order = c("RRDOCK", "PX_KL_SGE")) + 
       scale_color_manual(values = c("#6d9eeb", "#f6b26b")) +
       labs(x= "Card Type", 
            y= "Average Hydrocolor Measurement", 
            title= "Reflectance Card Comparisons for Hydrocolor 2.0",
            subtitle= "n = 34   n = 17") + ylim(0,15)

# save plots

ggsave("GC_comparison_bystation.png", width = 20, height = 20, units = "cm")

# ANOVA and statistical testing

res.aov <- aov(hydrocolor_means ~ cardType, data = all_gc_clean)
summary(res.aov) # see summary of analysis
TukeyHSD(res.aov)

# two way ANOVA

res.aov2 <- aov(hydrocolor_means ~ cardType + phoneModel, data = all_gc_clean)
summary(res.aov2)
TukeyHSD(res.aov2)

# means summary

all_gc_clean %>% group_by(stationName, cardType) %>% 
  summarise(hydrocolor_means=mean(hydrocolor_means),
            .groups = 'drop')

aggregate(all_gc_clean$hydrocolor_means, list(all_gc_clean$cardType), median)
aggregate(all_gc_clean$hydrocolor_means, list(all_gc_clean$stationName), median)
aggregate(all_gc_clean$hydrocolor_means, list(all_gc_clean$cardType), mean)
