# .csv data from http://www.slmpd.org/Crimereports.shtml

rm(list = ls())

setwd("/Users/noahbardash/Documents/GitHub/PS7")

library("dplyr")
library("ggplot2")
library("reshape")

crime_data <- read.csv("March2018.csv")

#2

crime_types <- select(crime_data, Description, DateOccur, Complaint, Neighborhood, District)
crime_types <- crime_types %>%
  mutate(homicide = as.numeric(grepl("homicide", Description, ignore.case=T)),
         DateOccur.new = substr(DateOccur, 1, 10),
         rape = as.numeric(grepl("rape", Description, ignore.case=T)),
         robbery = as.numeric(grepl("robbery", Description, ignore.case=T)),
         aggravated_assault = as.numeric(grepl("agg", Description, ignore.case=T)),
         burglary = as.numeric(grepl("burglary", Description, ignore.case=T)),
         larceny = as.numeric(grepl("larceny", Description, ignore.case=T)),
         vehicle_theft = as.numeric(grepl("auto theft", Description, ignore.case=T)),
         arson = as.numeric(grepl("arson", Description, ignore.case=T)),
         simple_assault = as.numeric(grepl("assault, ", Description, ignore.case=T))+
           as.numeric(grepl("simple assault", Description, ignore.case=T)),
         forgery = as.numeric(grepl("forgery", Description, ignore.case=T)),
         fraud = as.numeric(grepl("fraud", Description, ignore.case=T)),
         embezzlement = as.numeric(grepl("embezzlement", Description, ignore.case=T)),
         stolen = as.numeric(grepl("stolen", Description, ignore.case=T)),
         property_destruction = as.numeric(grepl("destruction of property", Description, ignore.case=T)),
         weapons = as.numeric(grepl("weapons", Description, ignore.case=T)),
         sex_offense = as.numeric(grepl("sex offns", Description, ignore.case=T)),
         drugs = as.numeric(grepl("drugs", Description, ignore.case=T)),
         dui = as.numeric(grepl("dui", Description, ignore.case=T)),
         liquor = as.numeric(grepl("liquor", Description, ignore.case=T)),
         disorderly_conduct = as.numeric(grepl("disorderly conduct", Description, ignore.case=T)),
         loitering = as.numeric(grepl("loitering", Description, ignore.case=T)),
         id = Complaint,
         neighborhood = Neighborhood,
         district = District)

View(crime_types)
names(crime_types)
crime_types <- crime_types[,c(6:30)]
temp <- melt(crime_types, id.var=c("id", "DateOccur.new", "neighborhood", "district"))
temp <- filter(temp, value==1)

crime_type_counts <- temp %>% 
group_by(variable, DateOccur.new) %>% 
  summarise(count=n())
View(crime_type_counts)

crime_type_counts2 <- temp %>% 
  group_by(variable) %>% 
  summarise(count=n())
View(crime_type_counts2)
# Larceny is the most common type of crime


#3
crime_neighborhood_counts <- temp %>% 
  group_by(DateOccur.new, neighborhood) %>% 
  summarise(count=n())
View(crime_neighborhood_counts)

crime_neighborhood_counts2 <- temp %>% 
  group_by(neighborhood) %>% 
  summarise(count=n())
View(crime_neighborhood_counts2)
# Neighborhood 35 has the most crime

#4
robberies_by_district <- temp %>%
  group_by(district) %>% 
  filter(variable == "robbery") %>%
  summarise(count=n())
crimes_by_district <- temp %>%
  group_by(district) %>% 
  summarise(count=n())
crimes_by_district <- crimes_by_district[-1,]
robberies_by_district$count/crimes_by_district$count
# District 5 has the greatest proportion of robberies

#5
class(crime_type_counts$DateOccur.new)
crime_type_counts$DateOccur.new <- as.Date(crime_type_counts$DateOccur.new, format="%m/%d/%Y")
threshold <- as.Date("02/01/2018", format="%m/%d/%Y")
crime_types_sub <- subset(crime_type_counts, crime_type_counts$DateOccur.new > threshold)
ggplot(crime_types_sub, aes(x=DateOccur.new, y = count, color=variable)) + geom_point()

#6

