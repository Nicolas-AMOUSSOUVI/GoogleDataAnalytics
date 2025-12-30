#Cyclistic Bike Share Case Study
#Introduction
#The Cyclistic Bike Share Case Study is a capstone project for the Google Data Analytics Professional Certificate on 
#Coursera. In this project, I will follow the data analysis process which I learned from the course: ask, prepare, 
#process, analyze, share and act to analyze the data.


#Background
#Cyclistic is a bike-share company based in Chicago that launched a successful bike-sharing program in 2016. Throughout
#the years, Throughout the years, the program has expanded significantly to a fleet of 5,824 bicycles and a network of 
# 692 geotracked stations sprawled across the city.With the large number of bicycles across numerous stations, customers can rent bikes from one station and 
#return them to any other station within the network at their convenience. This encourages people to opt for cycling as
#a mode of transportation, therefore contributing to the success of Cyclistic's bike-sharing program.

#Cyclistic's marketing strategy has so far focused on building general awareness and appealing to broad consumer
#segments. The company offers flexibile pricing plans that cater to diverse needs of users including single-ride passes,
#full-day passes, and annual memberships. Besides, it provides reclining bikes, hand tricycles, and cargo bikes, 
#effectively welcoming individuals with disabilities and those who can't ride on the standard two-wheeled bicycles. 
##Based on the company database, Cyclistic users are more likely to ride for leisure, but about 30% use them to commute
##to work each day. While traditional bikes remain as the popular option, around 8% of users opt for the assistive 
##alternatives.

#The company's marketing director believes that the company’s future success depends on maximizing the number of annual
#memberships. Therefore, as a junior data analyst, my team and I have to understand how casual riders and annual 
#members use Cyclistic bikes differently. From these insights, we will design a new marketing strategy to convert 
#casual riders into annual members.

#1. Ask
#Business Task - design marketing strategies to convert casual riders to members by understanding how annual and 
#casual riders differ, why casual riders would buy a membership, and how digital media could affect their marketing 
#tactics.

#Prepare
#Quick Links
#MIT License
#
# 3. Process
# The basis for this analysis is 2019 and 2020 Q1 data and the steps for processing the data are as follow:
# 
# Data Cleaning
# Data Combining
# Data Exploration
# Data Analysis

# The 2 tables were stacked and combined into a single table. The table consists of 791956 rows.
# 
# Data Exploration
# I ran the queries for each column from left to right in order to determine the data type and to uncover any missing values, outliers, inconsistencies, and errors within the dataset.
# 
# The data set consists of 13 variables, as shown in the following:
# 
# No.	Variable	Description
# 1	ride_id	Unique ID assigned to each ride
# 2	rideable_type	classic, docked, or electric
# 3	started_at	Date and time at the start of trip
# 4	ended_at	Date and time at the end of trip
# 5	start_station_name	Name of the station where the ride journey started from
# 6	start_station_id	ID of the station where the ride journey started from
# 7	end_station_name	Name of the station where the ride trip ended at
# 8	end_station_id	ID of the station where the ride trip ended at
# 9	start_lat	Latitude of starting station
# 10	start_lng	Longitude of starting station
# 11	end_lat	Latitude of ending station
# 12	end_lng	Longitude of ending station
# 13	usertype	Type of membership of each rider
# and the data type of each variable is depicted below:
# 
#Applicable Law and Forum. This Agreement is governed by the laws of the State of Illinois, without regard to conflicts of law principles. Any dispute arising under or relating to this Agreement will be brought only in a court of competent jurisdiction sitting in New York City, New York.
#Entire Agreement. This Agreement is the complete and exclusive agreement and understanding between Bikeshare and you with respect to its subject matter and supersedes all prior or contemporaneous oral or written agreements or understandings relating to the subject matter.

#Tools:
  
#Data cleaning & processing - Rstudio
#Data visualization - Rstudio
#Questions for guiding future marketing program:
  
# How do annual members and casual riders use Cyclistic bikes differently?
# Why would casual riders buy Cyclistic annual memberships?
# How can Cyclistic use digital media to influence casual riders to become members?

#
                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                   
# IMPORT
#install libraries
install.packages(c("tidyverse", "janitor", "lubridate", "gglpot2", "dplyr"))
install.packages("pandas")
##Importing libraries
library(tidyverse)
library(janitor)
library(lubridate)
library(ggplot2)
library(dplyr)

#inmporting the files
data2019 <- read.csv("C:/Users/nico/Desktop/Cours en ligne/Data science/Google certificate DAta analyst/Case studie 1/Divvy_Trips_2019_Q1.csv")
data2020 <- read.csv("C:/Users/nico/Desktop/Cours en ligne/Data science/Google certificate DAta analyst/Case studie 1/Divvy_Trips_2020_Q1.csv")

## Examining each dataset
str(data2019)
str(data2020)

# Checking number of rows
rowtotal <- nrow(data2019) + nrow(data2020)
rowtotal
print(rowtotal)

#Changing the col name of the data frame in order to have a uniform data 
colnames(data2020)[colnames(data2020) == "ride_id"] <- "trip_id"
colnames(data2020)[colnames(data2020) == "started_at"] <- "start_time"
colnames(data2020)[colnames(data2020) == "ended_at"] <- "end_time"
colnames(data2020)[colnames(data2020) == "ride_id"] <- "trip_id"
colnames(data2020)[colnames(data2020) == "start_station_name"] <- "from_station_name"
colnames(data2020)[colnames(data2020) == "start_station_id"] <- "from_station_id"
colnames(data2019)[colnames(data2019) == "to_station_id"] <- "end_station_id"
colnames(data2019)[colnames(data2019) == "to_station_name"] <- "end_station_name"
colnames(data2020)[colnames(data2020) == "member_casual"] <- "usertype"

str(data2020)
colnames(data2019)
colnames(data2020)

#removing useless col for homogeneity
data2020 <- subset(data2020, select = -c(rideable_type,start_lat,start_lng,end_lat,end_lng))
data2019 <- subset(data2019, select = -c(bikeid,birthyear,gender,tripduration))

#reorder column
data2019_reorder <- data2019[,c(1,2,3,5,4,7,6,8)]
colnames(data2019_reorder)
colnames(data2020)

##Combining the dataframe into one
data_combined <- merge(data2020,data2019_reorder, all = TRUE)
head(data_combined)
str(data_combined)

#Rows number on the combined data frame 
nrow(data_combined)

#DATA CLEANING
## the time format is YYYY-MM-DD HH:MM:SS to change the type
data_combined$start_time <- as.POSIXct(data_combined$start_time, format = "%Y-%m-%d %H:%M:%S")
data_combined$end_time <- as.POSIXct(data_combined$end_time, format = "%Y-%m-%d %H:%M:%S")
str(data_combined$start_time)

##remove the na
row_with_na <- data_combined[!complete.cases(data_combined),]
print(row_with_na)
data_combined <- data_combined[complete.cases(data_combined),]
## only one to remove 

#Rows number on the combined data frame 
nrow(data_combined)

#remove duplicate
data_no_duplicate <- data_combined[!duplicated(data_combined$trip_id), ]
print(paste("Removed", nrow(data_combined) - nrow(data_no_duplicate), "duplicate rows"))
## removed only 24 duplicates

##Data Manipulation
##Creating a column ride length 
data <- mutate(data_combined, ride_length = difftime(end_time, start_time, units = "mins"))

#check 
str(data)

#filtering trips with a ride length < 1 min
nrow(data[data$ride_length < 1, ])
data_v3 <- data[!data$ride_length < 1, ]

#check data_v3
glimpse(data_v3)

#change the term customer to casual and subscriber to member
data_v3$usertype[data_v3$usertype == "Customer"] <- "casual"
data_v3$usertype[data_v3$usertype == "Subscriber"] <- "member"

##Calculate the number of members vs casuals riders
#usertype_table <- table(data_v3$usertype)
#pie(usertype_table)

## determining the amount of members vs casual riders
#rider_type_total <- table(data_v3$usertype)
#View(rider_type_total)

#Pourcentage de membre ou casual
effectifs <- table(data_v3$usertype)
prop.table(effectifs) * 100

#Statistical analysis
trip_stat <- data_v3 %>% 
  group_by(usertype) %>% 
  summarise(average_rider_length = mean(ride_length), standard_deviation = sd(ride_length), median_ride_length = median(ride_length), min_ride_length = min(ride_length), max_ride_length = max(ride_length))
head(trip_stat)
## Warning the min ride_length is set a 1 min because shorter time could be malfunction
## max value are probably bike not docked successfully 
## casuals users use for longer time  89 min mean time while only 13 min for member

#Create separate col for the date, month, day, and day of the week
data_v4 <- data_v3
data_v4$date <- as.Date(data_v4$start_time)
data_v4$month <- format(as.Date(data_v4$date), "%b")
data_v4$day <- format(as.Date(data_v4$date), "%d")
data_v4$year <- format(as.Date(data_v4$date), "%Y")
data_v4$day_of_week <- format(as.Date(data_v4$date), "%A")

head(data_v4)

## Determine the mode for the day of the week (code learnt from tutorialspoint.com)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

weekday_mode <- getmode(data_v4$day_of_week)

print(weekday_mode)

#Pourcentage d'utilisateur pour chaque jour
effectifs <- table(data_v4$day_of_week)
prop.table(effectifs) * 100
#Users mainly take bikes during the week day ( 15 % approximately each day) while during weekend only (9%) 

#Pie chart is not so easy to read 
#pie(table(data_v4$day_of_week))

## Determining the most popular day by rider type
data_v4$day_of_week <- ordered(data_v4$day_of_week, levels = c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche"))
head(data_v4)

data_v4 %>% 
  group_by(usertype, day_of_week) %>% 
  summarise(rider_type_total = n(), average_ride_length = mean(ride_length)) %>% 
  arrange(usertype, day_of_week)
##Casuals users usually takes bikes thursday and friday for longer ride 140 and 103 respectivaly and the rest of the day 80 min
##Member users usually take bikes saturday and sunday for longer ride 16 min and the reste of the day 12 min
##Few casual users but long ride vs A lot of member users but short ride

## Determining the most popular months during 2020
popular_month <- data_v4 %>% 
  group_by(month) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(-number_of_rides)

View(popular_month)
##Lot of users mars january and february
## shorter ride during dec probably the temperature

## Determine the most popular start station
station_mode <- getmode(data_v4$from_station_name)
print(station_mode)

## Determine the most popular start station for members
popular_start_stations_member <- data_v4 %>% 
  filter(usertype == 'member') %>% 
  group_by(from_station_name) %>% 
  summarise(number_of_starts = n()) %>% 
  filter(from_station_name != "") %>% 
  arrange(- number_of_starts)

head(popular_start_stations_member)
##The most popular start station : Cana st & Adams St

## Determine the most popular start station for casual riders
popular_start_stations_casual <- data_v4 %>% 
  filter(usertype == 'casual') %>% 
  group_by(from_station_name) %>% 
  summarise(number_of_starts = n()) %>% 
  filter(from_station_name != "") %>% 
  arrange(- number_of_starts)

head(popular_start_stations_casual)
##The most popular start station :Streeter Dr & Grand Ave

## Determine the most popular end station
station_mode <- getmode(data_v4$end_station_name)
print(station_mode)

## Determine the most popular end station for members
popular_end_stations_member <- data_v4 %>% 
  filter(usertype == 'member') %>% 
  group_by(end_station_name) %>% 
  summarise(number_of_starts = n()) %>% 
  filter(end_station_name != "") %>% 
  arrange(-number_of_starts)

head(popular_end_stations_member)
##most popular end station : Canal St & Adams St

## Determine the most popular end station for casual riders
popular_end_stations_casual <- data_v4 %>% 
  filter(usertype == 'casual') %>% 
  group_by(end_station_name) %>% 
  summarise(number_of_starts = n()) %>% 
  filter(end_station_name != "") %>% 
  arrange(- number_of_starts)

head(popular_end_stations_casual)
##Most popular end station : Streeter Dr & Grand Ave

# DATA VISUALIZATION
## Visualization of the rider types
data_v4 %>% 
  group_by(usertype) %>% 
  summarise(total_rider_type = n()) %>% 
  ggplot(aes(x = usertype, y = total_rider_type, fill = usertype)) + 
  geom_col(position = "dodge") + geom_text(aes(label = total_rider_type, vjust = -0.25))

## Visualization of the user types ride duration
rider_type_average_duration <- data_v4 %>% 
  group_by(usertype) %>% 
  summarize(average_ride_length = mean(ride_length))

rider_type_average_duration %>% 
  ggplot(aes(x = usertype, y = average_ride_length, fill = usertype)) +
  geom_col(position = "dodge") + geom_text(aes(label = average_ride_length, vjust = -0.25))
##Casual use for 89 min and Member for 13 huge difference 

## Visualization of the usage by members and casual riders by the weekday
data_v4 %>% 
  group_by(usertype, day_of_week) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(usertype, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = usertype)) +
  geom_col(position = "dodge")

## Visualization of the number of trips by members and casual riders by the weekday
data_v4 %>% 
  group_by(usertype, day_of_week) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(usertype, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = usertype)) +
  geom_col(position = "dodge")

## Visualization of the usage by members and casual riders by the month
data_v4$month <- ordered(data_v4$month, levels=c("janv.", "févr.", "mars", "déc."))

data_v4 %>% 
  group_by(usertype, month) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length) ) %>% 
  arrange(usertype, month)  %>% 
  ggplot(aes(x = month, y = average_duration, fill = usertype)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = number_of_rides, angle = 90)) +
  facet_wrap(~usertype)

## Visualization of the number of trips by members and casual riders by the month
data_v4 %>% 
  group_by(usertype, month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(usertype, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = usertype)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = number_of_rides, angle = 90)) +
  facet_wrap(~usertype)

##this analyse was done with only data for décembre, jan, févr, mar


##Conclusion
##1.Casual use the service for long ride (89 min) a little more often thursday and friday but also during the rest of the week
##  Members use the service for short ride (13 min) and during all day of the week 
##  During cold month less use of the service

##2.Casual riders could buy annual memberships to go to work and different activity for short travel
##  Can we explain the long ride of casual user by the conditions of the subscribtion 
##  Member get free 15 min that why they have shorter ride length that why it's more for people living close to their activities

##3.Make ad promotion targeted for casual rider: tourist and people wanting to sightsee or nature escapade for explain on well knownen website
##  Member ad promoting short trip to jobs, activities and leisures alternative to the subway, bus or tram.
##  Make discount for students or companies to increase habits generation and get long time user
##  In order to convert casual to member we could give a free member day for the casual users in order to allow them to try the member experience.