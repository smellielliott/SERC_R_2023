# Test script for Fieldscope data manipulation
# 6_23_2023
# S. Elliott

#set directory

setwd("C:/Users/SGEll/OneDrive/Desktop/Hydrocolor_Fieldscope_R")

#load libraries

library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)
library(dplyr)

#set up data

raw_fieldscope <- read_csv("./RawData/All_07042023.csv")

# analyze data frame structure

print("raw fieldscope download")
print(raw_fieldscope)

# remove unwanted columns and rename FOR XLSX ONLY

hydrocolor2_data <- select(raw_fieldscope, -c(1, 4, 5, 6:8, 10:14, 19:24)) %>%
  rename("station" = "Station Name") %>% 
  rename("date" = "Observation Date") %>%
  rename("time" = "Study time (HH:MM)") %>% 
  rename("hydrocolor_1" = "Hydrocolor Turbidity - 1") %>%
  rename("hydrocolor_2" = "Hydrocolor Turbidity - 2") %>%
  rename("hydrocolor_3" = "Hydrocolor Turbidity - 3") %>%
  rename("phone_type" = "Type of phone") %>%
  rename("instrument" = "Instrument ID") %>%
  rename("ivch_1" = "In Vivo Chlorophyll 1") %>%
  rename("ivch_2" = "In Vivo Chlorophyll 2") %>%
  rename("ivch_3" = "In Vivo Chlorophyll 3") %>%
  rename("cdom_1" = "CDOM1") %>%
  rename("cdom_2" = "CDOM2") %>%
  rename("cdom_3" = "CDOM 3") %>%
  rename("turbidity_1" = "Turbidity 1") %>%
  rename("turbidity_2" = "Turbidity 2") %>%
  rename("turbidity_3" = "Turbidity 3")
print("hydrocolor 2.0 data")
print(hydrocolor2_data)

# remove unwanted columns and rename FOR CSV ONLY

hydrocolor2_data <- select(raw_fieldscope, -c(1, 4, 5, 6:8, 10:14, 19:24)) %>%
  rename("station" = "Station.Name") %>% 
  rename("date" = "Observation.Date") %>%
  rename("time" = "Study.time..HH.MM.") %>% 
  rename("hydrocolor_1" = "Hydrocolor.Turbidity...1") %>%
  rename("hydrocolor_2" = "Hydrocolor.Turbidity...2") %>%
  rename("hydrocolor_3" = "Hydrocolor.Turbidity...3") %>%
  rename("phone_type" = "Type.of.phone") %>%
  rename("instrument" = "Instrument.ID") %>%
  rename("ivch_1" = "In.Vivo.Chlorophyll.1") %>%
  rename("ivch_2" = "In.Vivo.Chlorophyll.2") %>%
  rename("ivch_3" = "In.Vivo.Chlorophyll.3") %>%
  rename("cdom_1" = "CDOM1") %>%
  rename("cdom_2" = "CDOM2") %>%
  rename("cdom_3" = "CDOM.3") %>%
  rename("turbidity_1" = "Turbidity.1") %>%
  rename("turbidity_2" = "Turbidity.2") %>%
  rename("turbidity_3" = "Turbidity.3")
print("hydrocolor 2.0 data")
print(hydrocolor2_data)

# create columns for averages/means and fill

hydrocolor2_data$hydrocolor_means <- apply(hydrocolor2_data[,4:6],1,mean)
hydrocolor2_data$ivch_means <- apply(hydrocolor2_data[,9:11],1,mean)
hydrocolor2_data$cdom_means <- apply(hydrocolor2_data[,12:14],1,mean)
hydrocolor2_data$turbidity_means <- apply(hydrocolor2_data[,15:17],1,mean)

# export to processed files folder as .xlsx

write_xlsx(hydrocolor2_data,"./ProcessedData/All_averages_07042023.xlsx")
