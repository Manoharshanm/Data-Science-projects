
###########################################################
#   Case study - Uber Supply Demand-Gap analysis          #
#   Name: Manohar Shanmugasundaram                        #
#   Roll Id: DDA1730068                                   #
###########################################################

# Import the required packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)

# Load data from the 'Uber Request Data.csv' file.
Uber <- read.csv("Uber Request Data.csv",stringsAsFactors = TRUE)

# see the structure of the Uber dataframe
str(Uber)

####################
# Data Cleaning
####################

# Replace the '/' with "-" in the date fields, so all the date value will be in the consistent 'dd-mm-yyyy' format
Uber$Request.timestamp <- gsub("\\/", "-", Uber$Request.timestamp)
Uber$Drop.timestamp <- gsub("\\/", "-", Uber$Drop.timestamp)

# Add the seconds to the timestamp values, so that the missing seconds are added with :00
Uber$Request.timestamp = paste0(Uber$Request.timestamp,":00")
Uber$Drop.timestamp = paste0(Uber$Drop.timestamp,":00")

# Convert the field to POSIX date timestamp format, any extra values added in the previous step will be stripped off
Uber$Request.timestamp = as.POSIXct(Uber$Request.timestamp, format = "%d-%m-%Y %H:%M:%S", na.rm = TRUE)
Uber$Drop.timestamp = as.POSIXct(Uber$Drop.timestamp, format = "%d-%m-%Y %H:%M:%S", na.rm = TRUE)


#################################
# Univariate & Segmented analysis
#################################

# 1. Univariate Analysis
# Analysis on the Status variable using the bar plot
ggplot(Uber, aes(x=Status, fill = Status)) + geom_bar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Plot Result - As per the plot, quite significant of the requests are either 'Cancelled' or 'No Cars Available'. 
# This needs to be analysed

# 2. Time interval (slots) analysis
####################################
# Segment the timelines under the buckets like below. so we do analysis based on the time intervals.
#   Time (from)   Time(To)    Group
#   00:00         04:59       Early Morning
#   05:00         11:59       Morning
#   12:00         15:59       Afternoon
#   16:00         20:59       Evening
#   21:00         23:59       Night
# In the cut logic the break is provided as 0 to 4 for "Early Morning", this include time from 00:00 to 04:59,
# the split is similar for the other groups

Uber$Request.timeclass <- cut(as.POSIXlt(Uber$Request.timestamp)$hour, 
                    breaks=c(0,4,11,15,20,23), 
                    labels=c("Early Morning","Morning","Afternoon","Evening","Night"),
                    include.lowest=T, na.rm = TRUE)

Uber$Drop.timeclass <- cut(as.POSIXlt(Uber$Drop.timestamp)$hour, 
                              breaks=c(0,4,11,15,20,23), 
                              labels=c("Early Morning","Morning","Afternoon","Evening","Night"),
                              include.lowest=T, na.rm = TRUE)

# Convert the timeclass to factor, so we can see the summary of the intervals
Uber$Request.timeclass <- factor(Uber$Request.timeclass)
Uber$Drop.timeclass <- factor(Uber$Drop.timeclass)

# Summary
summary(Uber)

# Plot to see the request on the time intervals
###############################################
# Analysis on the request time class (i.e., Morning, Afternoon, etc) variable using the bar plot
ggplot(Uber, aes(x=Request.timeclass)) + geom_bar(fill='maroon') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Analysis on the drop time class (i.e., Morning, Afternoon, etc) variable using the bar plot
ggplot(na.omit(Uber), aes(x=Drop.timeclass)) + geom_bar(fill='maroon') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Plot Result - As per the plot, 'Morning' and 'Evening' are timeslots where most trip requests are made.

# 2. Day analysis
#################
# Split the days (i.e, Monday, Tuesday, etc) and see any pattern on specific days from the Request and Drop timestamps
Uber$Request.weekday <- wday(Uber$Request.timestamp)
Uber$Drop.weekday <- wday(Uber$Drop.timestamp)

# Convert the numeric day returned from the wday to the textual day (i.e, 2 - Monday) using the map function
from <- c(1,2,3,4,5,6,7)
to <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
map = setNames(to, from)

# map function to change the day values to textual day
Uber$Request.weekday[] = map[Uber$Request.weekday]
Uber$Drop.weekday[] = map[Uber$Drop.weekday]

# convert to factor for analysis
Uber$Request.weekday <- factor(Uber$Request.weekday)
Uber$Drop.weekday <- factor(Uber$Drop.weekday)

# plot to check whether the trend of trips varies in each day of the week
ggplot(Uber, aes(x=Request.weekday, fill=Status)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Plot Result - There is no change in the trip trends on all weekdays. The trend seems same all the days.

# 3. Hour Split for Request
############################

# Get the hour from timestamp, this will be used for analysis the Gap for each hour
Uber$Request.hour <- hour(Uber$Request.timestamp)

# Box plot on hour and timeslots to see any outliers and percentiles
ggplot(Uber, aes(x=Request.timeclass, y=Request.hour)) + geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    scale_y_continuous (breaks = seq(0, 24, 4), limits=c(0, 24))

# Plot Result - No specific outliers are available in the trip data.

#################################
# Demand and Gap analysis
#################################

# Plot to see the status and the timelines to identify the problem timeslots for 'Cancelled' and 'No Cars Available' status
ggplot(Uber, aes(x=Request.timeclass, fill=Status)) + geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Plot Result - 'Morning', 'Evening' and 'Night' are the timeslots where most trips are either 'Cancelled' or 'No Cars Available'.

# Add a new column to get the count of records.
Uber$Val <- 1

# Copy the Uber dataset to another dataset for analysis
Uber_analysis <- Uber

# Combine the 'Cancelled' and 'No Cars Available' into a single data. 
# Since the both status needs to considered for Gap analysis
Uber_analysis$Status <- gsub("\\Cancelled", "Cancel/No Cars", Uber_analysis$Status)
Uber_analysis$Status <- gsub("\\No Cars Available", "Cancel/No Cars", Uber_analysis$Status)

# Identify the Request count for each timeslot, trip status and pickup point
demand_gap <- Uber_analysis %>% 
  group_by(Request.timeclass, Status, Pickup.point) %>% 
  summarise(Request.count = sum(Val))

# Identify the total request count for timeslot and pickup point (irrespective of trip status)
Total_req <- demand_gap %>% 
  group_by(Request.timeclass, Pickup.point) %>% 
  summarise(total = sum(Request.count))

# Convert the data into Wide format, so it will be useful for the plotting
demand_gap <- spread(demand_gap,Status,Request.count)

# Merge the total request count to original data set using the columns "Request.timeclass" and "Pickup.point" for the merge
demand_gap_merge <- merge(demand_gap,Total_req,by=c("Request.timeclass", "Pickup.point"))

########################
# Supply Demand-Gap Plot
########################

pd <- position_dodge(width = 0.2)

# Plot the supply Demand-Gap Plot. Use the aesthetics 'x' as timeslots
demand_plot <- ggplot(demand_gap_merge, aes(x=as.numeric(Request.timeclass))) 

# Add the aesthetics 'y' as 'Total trip requests', 'Trip Completed count' and 'Cancelled & No Cars Available trip counts' as 
# a separate line in plot
demand_plot <- demand_plot +
  geom_line(position=pd,aes(y=total), colour="blue") + geom_text(aes(label = total, y = total), size = 3) + 
  geom_line(position=pd,aes(y=`Trip Completed`), colour="green") + geom_text(aes(label = `Trip Completed`, y = `Trip Completed`), size = 3) + 
  geom_line(position=pd,aes(y=`Cancel/No Cars`), colour="red") + geom_text(aes(label = `Cancel/No Cars`, y = `Cancel/No Cars`), size = 3)

# Add the plot title and labels for x and y axis
demand_plot <- demand_plot +
  ggtitle("Supply Demand Gap") + labs(x = "Request Timeslots") + labs(y = "Request count")

# split the plot based on the Pickup point (Airport and City)
demand_plot <- demand_plot + facet_grid(. ~ Pickup.point) 

# Change the label for the x-axis values
demand_plot <- demand_plot + 
  scale_x_continuous(breaks=1:5, labels=c("Early Morning","Morning","Afternoon","Evening","Night"))

# Make the x axis lable appears as vertical to avoid overlapping
demand_plot <- demand_plot + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# Display the plot
demand_plot

# Plot Result: 
# For Trip from Airport - The 'Evening' and 'Night' timeslots have more Gap in the demand and supply.
# For Trip from City - The 'Morning' timeslot have more Gap in the demand and supply

######################################
# Supply Demand-Gap analysis (Hourly)
######################################

# Identify the Request count for each Request hour, trip status and pickup point
demand_gap_hourly <- Uber_analysis %>% 
  group_by(Request.timeclass, Request.hour, Status, Pickup.point) %>% 
  summarise(Request.count = sum(Val))

# Identify the total request count for request hour and pickup point (irrespective of trip status)
Total_req_hourly <- demand_gap_hourly %>% 
  group_by(Request.timeclass, Pickup.point,Request.hour) %>% 
  summarise(total = sum(Request.count))

# Convert the data into Wide format, so it will be useful for the plotting
demand_gap_hourly <- spread(demand_gap_hourly,Status,Request.count)

# Merge the total request count to original data set using the columns "Request.timeclass", "Request.hour" and "Pickup.point"
demand_gap_hourly_mrg <- merge(demand_gap_hourly,Total_req_hourly,by=c("Request.timeclass", "Pickup.point","Request.hour"))

# Remove the additional introduced column in the Uber dataset, it is not required any more.
Uber$Val <- NULL

##################################
# Supply Demand-Gap Plot (hourly)
##################################

# Plot the supply Demand-Gap Plot (hourly). Use the aesthetics 'x' as Request hour
demand_hourly_plot <- ggplot(demand_gap_hourly_mrg, aes(x=as.numeric(Request.hour))) 

# Add the aesthetics 'y' as 'Total trip requests', 'Trip Completed count' and 'Cancelled & No Cars Available trip counts' 
# as a separate line in plot
demand_hourly_plot <- demand_hourly_plot +
  geom_line(position=pd,aes(y=`Trip Completed`), colour="green") + geom_text(aes(label = `Trip Completed`, y = `Trip Completed`), size = 3) + 
  geom_line(position=pd,aes(y=total), colour="blue") + geom_text(aes(label = total, y = total), size = 3) + 
  geom_line(position=pd,aes(y=`Cancel/No Cars`), colour="red") + geom_text(aes(label = `Cancel/No Cars`, y = `Cancel/No Cars`), size = 3)

# Add the plot title and labels for x and y axis
demand_hourly_plot <- demand_hourly_plot +
  ggtitle("Supply Demand-Gap (Hourly)") + labs(x = "Request hours") + labs(y = "Request count")

# split the plot based on the Pickup point (Airport and City)
demand_hourly_plot <- demand_hourly_plot + facet_grid(. ~ Pickup.point) 

# Display the plot
demand_hourly_plot

# Plot Result: 
# For Trip from Airport - The supply is very low in Airport from 17:00 to 22:00 hours.
# For Trip from City - The supply is very low in City from 04:00 to 10:00 hours.

###################
#     End         #
###################