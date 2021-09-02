#Divvy_Exercise_fulll_Analysis 


# Google Data Analytics Certificate - Track 1 - Case study one.

# This is a guided case study from module 8 of the Google Data Analytics Certificate. 
# 
# Essential question - Case Study: How Does a Bike-Share Navigate Speedy Success? 
#   
#   - How are the different types of memberships using bikeshare programs represented in the data? 
#   - What information can we draw from historical data that can help us understand the difference between casual riders and riders with memberships?

# # # # # # # # # # # # # # # # # # # # # # # 
# Install required packages
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualization
# # # # # # # # # # # # # # # # # # # # # # #  

library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data

setwd("/Users/jordancreenaune/Documents/R")

df_202007 <- read_csv("202007-divvy-tripdata.csv")
df_202008 <- read_csv("202008-divvy-tripdata.csv")
df_202009 <- read_csv("202009-divvy-tripdata.csv")
df_202010 <- read_csv("202010-divvy-tripdata.csv")
df_202011 <- read_csv("202011-divvy-tripdata.csv")
df_202012 <- read_csv("202012-divvy-tripdata.csv")
df_202101 <- read_csv("202101-divvy-tripdata.csv")
df_202102 <- read_csv("202102-divvy-tripdata.csv")
df_202103 <- read_csv("202103-divvy-tripdata.csv")
df_202104 <- read_csv("202104-divvy-tripdata.csv")
df_202105 <- read_csv("202105-divvy-tripdata.csv")
df_202106 <- read_csv("202106-divvy-tripdata.csv")



# Combining all datasets
tripdata <- rbind(df_202007,
                  df_202008,
                  df_202009,
                  df_202010,
                  df_202011,
                  df_202012,
                  df_202101,
                  df_202102,
                  df_202103,
                  df_202104,
                  df_202105,
                  df_202106)
glimpse(tripdata)



# Remove rows with missing values
colSums(is.na(tripdata))

# missing values will be removed
tripdata_cleaned <- tripdata[complete.cases(tripdata), ]

# data with started_at greater than ended_at will be removed - remove possible inconsistencies
tripdata_cleaned <- tripdata_cleaned %>% 
  filter(tripdata_cleaned$started_at < tripdata_cleaned$ended_at)


# create new column `ride_length`
tripdata_cleaned$ride_length <- (tripdata_cleaned$ended_at - tripdata_cleaned$started_at)
head(tripdata_cleaned)


#Plot - demonstrating casual vs member riders for this data set 

number_of_rides <- tripdata_cleaned %>% 
  group_by(member_casual) %>% 
  summarize(number_of_rides=n())


ggplot(number_of_rides, aes(x=member_casual, y=number_of_rides)) + 
  geom_bar(stat = "identity",fill="dodgerblue3") + 
  labs(title = "Number of Rides July 2020 - July 2021") +
  ylab("Number of rides")


# create 2 new columns `day_of_week` and the 'day'

tripdata_cleaned$day_of_week <- wday(tripdata_cleaned$started_at, label = FALSE)
tripdata_cleaned$day <- weekdays(as.Date(tripdata_cleaned$started_at))


#Data Summary - Mean, Max, Min and Mode of ride length
# mean of ride_length
mean_ride_length <- tripdata_cleaned %>% 
  summarize(mean(ride_length))

# max ride_length
max_ride_length <- tripdata_cleaned %>% 
  summarize(max(ride_length))

# min ride_length
min_ride_length <- tripdata_cleaned %>% 
  summarize(min(ride_length))

#Mode of Column 
#Create a mode function 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculate the mode of day_of_week using the Mode function (from above)
day_mode <- getmode(tripdata_cleaned$day)
print(day_mode)
day_of_week_mode <- getmode(tripdata_cleaned$day_of_week)
#print(day_of_week_mode)

data_summary <- data.frame(max_ride_length,min_ride_length,mean_ride_length,day_of_week_mode)
print(data_summary)




# average ride_length for members and casual riders
average_d_o_w <- tripdata_cleaned %>% 
  group_by(day_of_week,day) %>% 
  summarize(mean(ride_length))

average_d_o_w$mean_ride_length <- round(average_d_o_w$`mean(ride_length)` ,digit=2)
average_d_o_w <- average_d_o_w[ -c(3) ]

print(average_d_o_w)


#Visualisation - Visualisation of time series data
#Order data by date and time, isolate one column

date_order <- tripdata_cleaned[order(tripdata_cleaned$started_at),3,drop=FALSE ]
head(date_order) #Check data is in the correct order 

#Date order plot 

date_order_plot <-  date_order %>%  
  group_by(started_at) %>% 
  summarise(session_count = n()) %>% 
  ggplot(aes(started_at, session_count)) + 
  geom_smooth()+
  labs(x = "Time of year", y = "Number_of_Rides - Hundreds of Thousands", 
       title = "Number of Rides July 2020 - July 2021")

date_order_plot


#Number of rides for users by day_of_week
tripdata_cleaned %>% 
  group_by(ride_id, day_of_week) %>% 
  summarize(number_of_rides=n())

head(tripdata_cleaned)


# average ride_length by type and day of week
counts <- aggregate(tripdata_cleaned$ride_length ~ tripdata_cleaned$member_casual +
                      tripdata_cleaned$day_of_week + tripdata_cleaned$day, FUN = mean)
names(counts)[1] <- "member_casual"
names(counts)[2] <- "day_of_week"
names(counts)[3] <- "day"
names(counts)[4] <- "mean_ride_length"

counts$mean_ride_length <- round(counts$mean_ride_length,digit=2)
counts <- counts[ -c(2) ]
counts$day <- factor(counts$day, levels= c("Sunday", "Monday", 
                                           "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
counts <- counts[order(counts$day), ]

#Check count of ride_length of both kinds of riders 
head(counts)


#Average ride time by each day for members vs casual users
average_casual_vs_member <- 
  aggregate(tripdata_cleaned$ride_length ~ tripdata_cleaned$member_casual + tripdata_cleaned$day, FUN = mean)
names(average_casual_vs_member)[1] <- "member_casual"
names(average_casual_vs_member)[2] <- "day_of_week"
names(average_casual_vs_member)[3] <- "mean_ride_length"
average_casual_vs_member$mean_ride_length <- round(average_casual_vs_member$`mean_ride_length` ,digit=2)
printrows <- average_casual_vs_member[1:14,]
printrows



#Analyze and visualise ridership data by type and weekday
tripdata_cleaned %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)


# visualize number of rides by rider type
tripdata_cleaned %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#CC6633","#6699CC")) +
  labs(title = "Number of Rides by Days and Rider Type",
       subtitle = "Members versus Casual Users") +
  ylab("Number of  - Hundreds of thousands") +
  xlab("Day of Week")


#Visualization for average duration - with regard to membership status (casual vs member)

tripdata_cleaned %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(average_duration = mean(ride_length)/60) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#CC6633","#6699CC")) +
  labs(title = "Average Duration of Rides by Days and Rider Type",
       subtitle = "Members versus Casual Users in minutes") +
  ylab("Average Duration of Rides- minutes") +
  xlab("Day of Week")


#Average ride_length and type and month
tripdata_cleaned$month <- month(tripdata_cleaned$started_at, label = TRUE)
rides <- aggregate(tripdata_cleaned$ride_length ~ tripdata_cleaned$member_casual +
                     tripdata_cleaned$month,FUN = mean)

#Casual Riders time of day 

casual_rider <- subset(tripdata_cleaned, member_casual == "casual")
#separate date and time into two columns 
casual_time <- separate(casual_rider, started_at, into = c("date", "time"), sep = " ",)
#Drop non essential columns 
casual_rider_time <- casual_time[order(casual_time$time),4,drop=FALSE ]
#Aggregate time and count instances in a new column
casual_time_count <- casual_rider_time %>% count(time)

#Plot - Number of riders - time of day Casual Users
ggplot() +
  geom_point(data=casual_time_count, aes(time, n),colour="red")+
  update_geom_defaults("point",list(size=0.2))+
  labs(x = "Time of Day 00:00 to 11:59 - Casual Users", y = "Number of rides", 
       title = "Number of rides and time of day")

#Member Riders time of day 

member_rider <- subset(tripdata_cleaned, member_casual == "member")
#separate date and time into two columns 
member_time <- separate(member_rider, started_at, into = c("date", "time"), sep = " ",)
#Drop non essential columns 
member_rider_time <- member_time[order(member_time$time),4,drop=FALSE ]
#Aggregate time and count instances in a new column
member_time_count <- member_rider_time %>% count(time)



#Plot - Number of riders - time of day membership Users
ggplot() +
  geom_point(data=member_time_count, aes(time, n),colour="blue")+
  update_geom_defaults("point",list(size=0.2))+
  labs(x = "Time of Day 00:00 to 11:59 Membership users", y = "number of rides", 
       title = "Number of rides and time of day")







#Dataset is ordered and exported to a csv for further analysis to be imported to tableau or PowerBI

alltrips <- tripdata_cleaned %>% 
  select(-day_of_week)
alltrips$day_of_week <- wday(alltrips$started_at, label = TRUE)

alltrips_ordered <- alltrips[order(alltrips$started_at),]

head(alltrips_ordered )






