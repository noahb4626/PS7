# .csv data from http://www.slmpd.org/Crimereports.shtml

rm(list = ls())

library("dplyr")
library("ggplot2")

setwd("/Users/noahbardash/Documents/GitHub/PS7")
crime_data <- read.csv("March2018.csv")

crime_types <- select(crime_data, Description, DateOccur)
crime_types$DateOccur <- substr(crime_types$DateOccur, 1, 10)

crimesByDay <- crime_types %>% 
  group_by(DateOccur, Description) %>%
  summarise(count=n())
View(crimesByDay)

