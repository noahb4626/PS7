# Download .csv data from http://www.slmpd.org/Crimereports.shtml

# Clear global environment
rm(list = ls())

# Set working directory
setwd("/Users/noahbardash/Documents/GitHub/PS7")

# Load necessary packages
library("dplyr")
library("ggplot2")
library("reshape")

# Read in March 2018 crime data
crime_data <- read.csv("March2018.csv")

#2

# Don't need all the information in crime_data, select the useful columns
crime_types <- select(crime_data, Description, DateOccur, Complaint, Neighborhood, District)
# Clean description category to group by major types of crime (e.g. larceny, arson)
crime_types <- crime_types %>%
  mutate(homicide = as.numeric(grepl("homicide", Description, ignore.case=T)),
         date = substr(DateOccur, 1, 10), # only want MM/DD/YYYY for sorting by date, not timestamp
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
crime_types <- crime_types[,c(6:30)] # don't need first 5 columns of crime_types (they are not actually categories of crime)
# Condense crime type information (T/F information for 25 types to a variable corresponding to each type)
temp <- melt(crime_types, id.var=c("id", "date", "neighborhood", "district"))
temp <- filter(temp, value==1)
View(temp)

# Number of crimes per day by type of crime using group_by
crime_type_counts <- temp %>% 
group_by(variable, date, district) %>% 
  summarise(count=n())
View(crime_type_counts)

# Display crime counts for each general category of crime over entire timespan
crime_type_counts2 <- temp %>% 
  group_by(variable) %>% 
  summarise(count=n())
View(crime_type_counts2)
# Larceny is the most common type of crime


#3
# Number of crimes per day by neighborhood using group_by
crime_neighborhood_counts <- temp %>% 
  group_by(date, neighborhood) %>% 
  summarise(count=n())
View(crime_neighborhood_counts)

# Display crime counts for each neighborhood over entire timespan
crime_neighborhood_counts2 <- temp %>% 
  group_by(neighborhood) %>% 
  summarise(count=n())
View(crime_neighborhood_counts2)
# Neighborhood 35 has the most crime

#4
# Calculate total robberies by district
robberies_by_district <- temp %>%
  group_by(district) %>% 
  filter(variable == "robbery") %>%
  summarise(count=n())
# Calculate total crimes by district
crimes_by_district <- temp %>%
  group_by(district) %>% 
  summarise(count=n())
crimes_by_district <- crimes_by_district[-1,] # District 0 has 10 crimes but 0 robberies, so proportion of 0. Remove from dataset
robberies_by_district$count/crimes_by_district$count # Robberies divided by total crime count for proportion by district
# District 5 has the greatest proportion of robberies

#5
crime_type_counts$date <- as.Date(crime_type_counts$date, format="%m/%d/%Y") # date is currently a set of characters, need to set class to Date
threshold <- as.Date("02/01/2018", format="%m/%d/%Y") # we only want to display data from Feb. 1 onward (vast majority of dataset included here, makes plot more visually appealing)
crime_types_sub <- subset(crime_type_counts, crime_type_counts$date > threshold) # subset data from Feb. 1 onward
ggplot(crime_types_sub, aes(x=date, y = count, color=variable)) + geom_point() + labs(title = "STL crime over time by category") # create plot of crime data over time by type of crime


#6
ggplot(crime_types_sub, aes(x=date, y = count, color=district)) + geom_point() + labs(title = "STL crime over time by district") # create plot of crime data over time by district

