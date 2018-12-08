###########################################################
#     NYC Parking Tickets - Group Case Study Assignment   #
###########################################################

# Group members 
# Shivam Kakkar (Facilitator) - Roll Number - DDA1730346
# Ashwin Suresh
# Manohar Shanmugasundaram
# P Sai Prathyusha

################################
# Loading the required libraries
################################
library(SparkR)
library(ggplot2)
library(gridExtra)

# initiating the spark session
sparkR.session(master = "local",appName="SparkR")

nyc_parking_2015 <- read.df("s3://skakkar-bucket/group-assignment/Parking_Violations_Issued_-_Fiscal_Year_2015.csv",source = "csv", inferSchema = "true", header = "true",na.strings="")
nyc_parking_2016 <- read.df("s3://skakkar-bucket/group-assignment/Parking_Violations_Issued_-_Fiscal_Year_2016.csv",source = "csv", inferSchema = "true", header = "true",na.strings="")
nyc_parking_2017 <- read.df("s3://skakkar-bucket/group-assignment/Parking_Violations_Issued_-_Fiscal_Year_2017.csv",source = "csv", inferSchema = "true", header = "true",na.strings="")

dim(nyc_parking_2015) # 11809233 , 51
dim(nyc_parking_2016) # 10626899 , 43
dim(nyc_parking_2017) # 10803028 , 43

printSchema(nyc_parking_2015)
printScehma(nyc_parking_2016)
printSchema(nyc_parking_2017)

# Fucntion to clean the data and prepare it for analysis:
# This function will be called upon on all the respective datasets for 2015 , 2016 and 2017

clean_prepare_data <- function(df , year) {
  
  # subsetting only the following columns as they are only required for analysis:
  
  # "Summons Number",
  # "Registration State"
  # "Issue Date","Violation Code",
  # "Vehicle Body Type",
  # "Vehicle Make",
  # "Violation Location",
  # "Violation Precinct",
  # "Issuer Precinct",
  # "Violation Time",
  # "House Number",
  # "Street Name"
  
  df <- select(df,"Summons Number","Registration State","Issue Date","Violation Code",
               "Vehicle Body Type","Vehicle Make","Violation Location","Violation Precinct",
               "Issuer Precinct","Violation Time","House Number","Street Name")
  
  # Droppng duplicate rows in the data-frame
  
  df<-dropDuplicates(df)
  
  # Converting the issue date to date type
  
  df$`Issue Date` = to_date(df$`Issue Date` , 'MM/dd/yyyy')
  
  # creating an year column from the issue date
  
  df <- mutate(df , Year = year(df$`Issue Date`))
  
  # creating a month column from the issue date
  
  df <- mutate(df , Month = month(df$`Issue Date`))
  
  # We are considering the fiscal year: on the basis of it we are filterng the records
  # For 2015 -   Issue date should be in between 1 July, 2014 - 30th June 2015
  # For 2016 -   Issue date should be in between 1 July, 2015 - 30th June 2016
  # For 2017 -   Issue date should be in between 1 July, 2016 - 30th June 2017
  
  df <- filter(df , (df$Month %in% c(7,8,9,10,11,12) & df$Year == year-1) |
                 (df$Month %in% c(1,2,3,4,5,6) & df$Year == year))
  return(df)
}

nyc_parking_2015 <- clean_prepare_data(nyc_parking_2015,2015)
cache(nyc_parking_2015)

nyc_parking_2016 <- clean_prepare_data(nyc_parking_2016,2016)
cache(nyc_parking_2016)

nyc_parking_2017 <- clean_prepare_data(nyc_parking_2017,2017)
cache(nyc_parking_2017)


#Function to check distinct rows and nulls in different columns

count_df <- function(df) {
  # count of (distinct rows,Nulls in Violation Time,Nulls in House Number,Nulls in Street Name
  #            Nulls in Violation Code,Nulls in Violation Precinct,Nulls in Year, Nulls in Month,
  #             Nulls in Summons Number)
  out <- numeric(9)
  out[1] <- count(distinct(df))
  out[2] <- count(where(df,isNull(df$`Violation Time`))) 
  out[3] <- count(where(df,isNull(df$`House Number`)))
  out[4] <- count(where(df,isNull(df$`Street Name`)))
  out[5] <- count(where(df,isNull(df$`Violation Code`)))
  out[6] <- count(where(df,isNull(df$`Violation Precinct`)))
  out[7] <- count(where(df,isNull(df$`Year`)))
  out[8] <- count(where(df,isNull(df$`Month`)))
  out[9] <- count(where(df,isNull(df$`Summons Number`)))
  out
}

#Output counts

count_df(nyc_parking_2015)

# Overall      Violation time  House Number  Street Name Violation Code Violation Precinct Year  Month   Summons Number 
# -------      --------------  ------------  ----------- -------------- ------------------ ----  -----   --------------
# 10598035     1438            1620679       5156          0              0                 0      0        0

count_df(nyc_parking_2016) 

# Overall      Violation time  House Number  Street Name Violation Code Violation Precinct Year  Month   Summons Number 
# -------      --------------  ------------  ----------- -------------- ------------------ ----  -----   --------------
# 10396894      716            1962227       4482        0              1                  0    0        0

count_df(nyc_parking_2017)

# Overall      Violation time  House Number  Street Name Violation Code Violation Precinct Year  Month   Summons Number 
# -------      --------------  ------------  ----------- -------------- ------------------ ----  -----   --------------
# 10539563       53            2159447       3745        0               0                  0        0        0
#############

# Check for wrong entries in year.Found no such records


Issue_date_year<- function(df) {
  agg(groupBy(df, df$Year), different_years_count = n(df$Year)) 
}

head(Issue_date_year(nyc_parking_2015))

# Year       different_years_count
#  2015               5373971
#  2014               5224064

head(Issue_date_year(nyc_parking_2016))

# Year        different_years_count
#  2015               5526176
#  2016               4870718


head(Issue_date_year(nyc_parking_2017))

# Year         different_years_count
#  2016               5109661
#  2017               5429902
##############

# Check for wrong entries in month.Found no such records

Issue_date_month<- function(df) {
  agg(groupBy(df, df$Month), different_months_count = n(df$Month)) 
}


diff_month_2015<-Issue_date_month(nyc_parking_2015)
showDF(diff_month_2015)

# -----------------+
# |Month|different_months_count|
# +-----+----------------------+
# |   12|                671343|
# |    1|                777887|
# |    6|               1004087|
# |    3|                957743|
# |    5|                984991|
# |    9|                954744|
# |    4|                918253|
# |    8|                884733|
# |    7|                949486|
# |   10|                966232|
# |   11|                797526|
# |    2|                731010|
# +-----+----------------------+



diff_month_2016<-Issue_date_month(nyc_parking_2016)
showDF(diff_month_2016)

# |Month|different_months_count|
# +-----+----------------------+
# |   12|                767085|
# |    1|                814181|
# |    6|                427117|
# |    3|               1013888|
# |    5|                874469|
# |    9|                939355|
# |    4|                900709|
# |    8|                902634|
# |    7|                884785|
# |   10|               1096952|
# |   11|                935365|
# |    2|                840354|
# +-----+----------------------+




diff_month_2017<-Issue_date_month(nyc_parking_2017)
showDF(diff_month_2017)

# |Month|different_months_count|
# +-----+----------------------+
# |   12|                778704|
# |    1|                877365|
# |    6|                852187|
# |    3|                964737|
# |    5|               1020244|
# |    9|                960537|
# |    4|                888402|
# |    8|                801258|
# |    7|                700475|
# |   10|                969330|
# |   11|                899357|
# |    2|                826967|
# +-----+----------------------+

###################################################################################################################################

###################
# Examine the data.
###################

################################################
# 1. Find total number of tickets for each year.
################################################

get_tot_tickets <- function(df) {
  head(select(df, countDistinct(df$`Summons Number`)))
}

#################################################################################
# 2. Find out how many unique states the cars which got parking tickets came from.
#################################################################################

get_cnt_uniq_states <- function(df) {
  createOrReplaceTempView(df, "df")
  unique_no_of_states <- SparkR::sql("select count(distinct(`Registration State`)) 
                                     AS distinct_no_of_states from df")
  head(unique_no_of_states)
}


############################################################################################################################
#3. Some parking tickets don’t have addresses on them, which is cause for concern. Find out how many such tickets there are.
############################################################################################################################

get_cnt_empty_addr_tickets <- function(df) {
  createOrReplaceTempView(df, "df")
  df <- SparkR::sql("select count(`Summons Number`) AS `no_of_empty_address` 
                    from df where `House Number` IS NULL  OR `Street Name` IS NULL")
  head(df)
}

###################
# Aggregation tasks
###################

#############################################################################################
# 1. How often does each violation code occur? (frequency of violation codes - find the top 5)
#############################################################################################

get_top_5_violaton_code_freq <- function(df) {
  violation_code_freq <- summarize(groupBy(df, df$`Violation Code`),
                                   count = n(df$`Violation Code`))
  violation_code_freq_desc <-  arrange(violation_code_freq, desc(violation_code_freq$count))
  top_5_violation_code_freq <- take(violation_code_freq_desc , 5)
}

# Following function is required in question # 5.4

get_top_3_violaton_codes <- function(df) {
  violation_code_freq <- summarize(groupBy(df, df$`Violation Code`),
                                   count = n(df$`Violation Code`))
  violation_code_freq_desc <-  arrange(violation_code_freq, desc(violation_code_freq$count))
  top_3_violation_codes <- take(violation_code_freq_desc , 3)$`Violation Code`
}

######################################################################################################################
# 2. How often does each vehicle body type get a parking ticket? How about the vehicle make? (find the top 5 for both)
######################################################################################################################

get_top_5_vehicle_body_type_freq <- function(df) {
  vehicle_body_type_freq <- summarize(groupBy(df, df$`Vehicle Body Type`),
                                      count = n(df$`Vehicle Body Type`))
  vehicle_body_type_desc <-  arrange(vehicle_body_type_freq, desc(vehicle_body_type_freq$count))
  top_5_vehicle_body_type_freq <- take(vehicle_body_type_desc , 5)
}

get_top_5_vehicle_make_freq <- function(df) {
  vehicle_make_freq <- summarize(groupBy(df, df$`Vehicle Make`),
                                 count = n(df$`Vehicle Make`))
  vehicle_make_desc <-  arrange(vehicle_make_freq, desc(vehicle_make_freq$count))
  top_5_vehicle_make_freq <- take(vehicle_make_desc , 5)
}

###########################################################################################
# 3. A precinct is a police station that has a certain zone of the city under its command. 
# 3.1 
# Find the (5 highest) frequencies of:
# Violating Precincts (this is the precinct of the zone where the violation occurred)
###########################################################################################

get_top_5_violating_precincts_freq <- function(df) {
  violating_precincts_freq <- summarize(groupBy(df, df$`Violation Precinct`),
                                        count = n(df$`Violation Precinct`))
  violating_precincts_freq_desc <- arrange(violating_precincts_freq, desc(violating_precincts_freq$count))
  top_5_violating_precincts_freq <- take(violating_precincts_freq_desc , 5)
}

###################################################################
# 3.2 
# Find the (5 highest) frequencies of:
# Issuing Precincts (this is the precinct that issued the ticket)
###################################################################

get_top_5_issuing_precincts_freq <- function(df) {
  issuing_precincts_freq <- summarize(groupBy(df, df$`Issuer Precinct`),
                                      count = n(df$`Issuer Precinct`))
  issuing_precincts_freq_desc <- arrange(issuing_precincts_freq, desc(issuing_precincts_freq$count))
  top_5_issuing_precincts_freq <- take(issuing_precincts_freq_desc , 5)
}

# Retrieving top 3 issuing precincts

get_top_3_issuing_precincts <- function(df) {
  issuing_precincts_freq <- summarize(groupBy(df, df$`Issuer Precinct`),
                                      count = n(df$`Issuer Precinct`))
  issuing_precincts_freq_desc <- arrange(issuing_precincts_freq, desc(issuing_precincts_freq$count))
  top_3_issuing_precincts <- take(issuing_precincts_freq_desc , 3)$`Issuer Precinct`
}

##########################################################################################################
# 4. 
# Find the violation code frequency across 3 precincts which have issued the most number of tickets - 
# do these precinct zones have an exceptionally high frequency of certain violation codes? 
# Are these codes common across precincts?
##########################################################################################################

get_violation_code_freq_top_3_precincts <- function(df , top_3_issuing_precincts) {
  violation_code_freq_top_3_precincts <- filter(df , df$`Issuer Precinct` %in% 
                                                  top_3_issuing_precincts)
  violation_code_freq_top_3_precincts <- summarize(groupBy(violation_code_freq_top_3_precincts ,
                                                           violation_code_freq_top_3_precincts$`Issuer Precinct`,
                                                           violation_code_freq_top_3_precincts$`Violation Code`), 
                                                   count_val = n(violation_code_freq_top_3_precincts$`Violation Code`))
  
  createOrReplaceTempView(violation_code_freq_top_3_precincts , "violation_code_freq_top_3_precincts")    
  violation_code_freq_top_3_precincts <- SparkR::sql("SELECT `Issuer Precinct`, `Violation Code`, count_val, rank FROM 
                                                     ( SELECT `Issuer Precinct`, `Violation Code`, count_val, rank() 
                                                     OVER (PARTITION BY `Issuer Precinct` ORDER BY count_val DESC) as rank 
                                                     from violation_code_freq_top_3_precincts) tmp WHERE rank <= 1 ")
  head(violation_code_freq_top_3_precincts)  
}


##################################################################################################
# 5 .You’d want to find out the properties of parking violations across different times of the day:
##################################################################################################

# 5.1
# The Violation Time field is specified in a strange format. 
# Find a way to make this into a time attribute that you can use to divide into groups

create_new_attribute_violation_time <- function(df) {
  df  <- mutate(df , violation_hour = substr(df$`Violation Time` , 1 ,2) , 
                violation_min = substr(df$`Violation Time` , 3 ,4) , 
                violation_clock = substr(df$`Violation Time` , 0 ,1))
  
  df <- mutate(df , violation_hour = cast(df$violation_hour , "integer"))
  df <- mutate(df , violation_min = cast(df$violation_min , "integer"))
  
  df$violation_hour <- ifelse(df$violation_clock == "P" , 
                              ifelse(df$violation_hour == 12 , df$violation_hour , df$violation_hour + 12) , 
                              df$violation_hour)  
  return(df)
}

# 5.2 Find a way to deal with missing values, if any.

# getting the missing values

get_tot_missing_values <- function(df) {
  tot_na <- sum(ifelse(isNull(df$violation_hour),1,0)) 
}

# Replacing the missing the mean of the hour if required.

replace_missing_values <- function(df , value) {
  
  # So we can replace the missing value with value 
  
  df$violation_hour <- ifelse(isNull(df$violation_hour) , value , df$violation_hour)
  return(df)
}

# 5.3
# Divide 24 hours into 6 equal discrete bins of time. The intervals you choose are at your discretion. 
# For each of these groups, 
# find the 3 most commonly occurring violations

# Now creating 6 bins 
# Pre Morning -> 0-4
# Early Morning -> 4 - 8
# Afternoon -> 8 - 12
# Evening -> 12 - 16
# Late Evening -> 16 - 20
# Night -> 20 - 00

create_bins <- function(df) {
  df$violation_slot <- ifelse(df$violation_hour < 4, "Pre_Morning", 
                              ifelse(df$violation_hour < 8 ,"Early Morning",
                                     ifelse(df$violation_hour < 12,"Afternoon",
                                            ifelse(df$violation_hour < 16,"Evening",
                                                   ifelse(df$violation_hour < 20,"Late Evening", "Night")))))
  return(df)
  
}

# For each of these groups, find the 3 most commonly occurring violations

three_most_common_violations <- function(df) {
  violations_in_diff_day_slots <- summarize(groupBy(df, df$violation_slot , df$`Violation Code`),
                                            count = n(df$`Violation Code`))
  createOrReplaceTempView(violations_in_diff_day_slots, "violations_in_diff_day_slots")
  
  violations_in_diff_day_slots <- SparkR::sql("SELECT Violation_slot, `Violation Code`,count, rank
                                              FROM ( SELECT Violation_slot, `Violation Code`, count,
                                              rank() OVER (PARTITION BY violation_slot ORDER BY count DESC) as rank
                                              FROM violations_in_diff_day_slots) tmp
                                              WHERE rank <= 3")
  head(violations_in_diff_day_slots , n = 18)
}


# 5.4
# Now, try another direction. For the 3 most commonly occurring violation codes, 
# find the most common times of day (in terms of the bins from the previous part)

get_most_common_times_for_top_3_violation <- function(df , top_3_violation_codes) {
  times_for_top_3_violation <- filter(df , df$`Violation Code` %in% 
                                        top_3_violation_codes)
  times_for_top_3_violation <- summarize(groupBy(times_for_top_3_violation ,
                                                 times_for_top_3_violation$`Violation Code`,
                                                 times_for_top_3_violation$Violation_slot), 
                                         count_val = n(times_for_top_3_violation$Violation_slot))
  
  createOrReplaceTempView(times_for_top_3_violation , "times_for_top_3_violation")    
  times_for_top_3_violation <- SparkR::sql("SELECT `Violation Code`, `Violation_slot`, count_val, rank 
                                           FROM (SELECT `Violation Code`, `Violation_slot`, count_val, rank() 
                                           OVER (PARTITION BY `Violation Code` ORDER BY count_val DESC) as rank 
                                           FROM times_for_top_3_violation) tmp 
                                           WHERE rank <= 3 ")
  head(times_for_top_3_violation , n = 9)  
}


######################################################################################################
# 6. Let’s try and find some seasonality in this data
# First, divide the year into some number of seasons, and find frequencies of tickets for each season.
# Then, find the 3 most common violations for each of these season
#######################################################################################################

# Assign the seasonality as below
# Winter - December, Janurary, Feburary
# Spring - March, April, May 
# Summer - June, July, August
# Autumn - September, October, November

# 6.1 First, divide the year into some number of seasons, and 
# find frequencies of tickets for each season.

creating_seasons_from_year <- function(df) {
  df$season <- ifelse((df$Month == 12 | df$Month == 1 | df$Month == 2), "Winter", 
                      ifelse((df$Month == 3 | df$Month == 4 | df$Month == 5) ,"Spring",
                             ifelse((df$Month == 6 | df$Month == 7 | df$Month == 8), "Summer", "Autumn")))
  return(df)
  
}

get_freq_tickets_season <- function(df) {
  head(summarize(groupBy(df, df$season),
                 count = n(df$season)))
}

# 6.2

# 3 most commonly occuring volations for each of the season

three_most_common_violations_season <- function(df) {
  violation_season_in_diff_slots <- summarize(groupBy(df, df$season , df$`Violation Code`),
                                              count = n(df$`Violation Code`))
  createOrReplaceTempView(violation_season_in_diff_slots, "violation_season_in_diff_slots")
  
  df <- SparkR::sql("SELECT season,`Violation Code`,count,rank
                    FROM (SELECT season, `Violation Code`, count, rank() 
                    OVER (PARTITION BY season ORDER BY count DESC) as rank
                    FROM violation_season_in_diff_slots) tmp
                    WHERE
                    rank <= 3")
  
  head(df , n = 12)
}

#############################################################################################################################################################################
# 7. The fines collected from all the parking violation constitute a revenue source for the NYC police department.
# Let’s take an example of estimating that for the 3 most commonly occurring codes.
#
# a) Find total occurrences of the 3 most common violation codes
# b) Then, search the internet for NYC parking violation code fines. You will find a website (on the nyc.gov URL) that lists these fines.
#    They’re divided into two categories, one for the highest-density locations of the city, the other for the rest of the city. For simplicity, take an average of the two.
# c) Using this information, find the total amount collected for all of the fines. State the code which has the highest total collection.
# d) What can you intuitively infer from these findings?
############################################################################################################################################################################

top3_violation_codes_fines <- function(df,fines) {
  df$total_fine_amt <- 0
  for(i in 1:nrow(df)){
    df$total_fine_amt[i] <- df[(i),2] * fines[i]}
  return(df)
}

# We have addressed b , c and d question as part of analysis of year 2015 , 2016 and 2017 respectively

######################################################################################################

########################
# Analysis For year 2015
# ######################

total_tickets_2015 <- get_tot_tickets(nyc_parking_2015)
total_tickets_2015 #10598035

unique_states_2015 <- get_cnt_uniq_states(nyc_parking_2015)
unique_states_2015 #69

cnt_empty_addr_2015 <- get_cnt_empty_addr_tickets(nyc_parking_2015)
cnt_empty_addr_2015 #1622076

# Aggregate - Question 1
top_5_violation_code_freq_2015 <- get_top_5_violaton_code_freq(nyc_parking_2015)
top_5_violation_code_freq_2015

#Violation  Code   count
#1             21 1469228
#2             38 1305007
#3             14  908418
#4             36  747098
#5             37  735600

# Aggregate - Question 2
top_5_vehicle_body_type_freq_2015 <- get_top_5_vehicle_body_type_freq(nyc_parking_2015)
top_5_vehicle_body_type_freq_2015

#Vehicle Body   Type   count
#1              SUBN 3341110
#2              4DSD 3001810
#3               VAN 1570227
#4              DELV  822040
#5               SDN  428571

top_5_vehicle_make_freq_2015 <- get_top_5_vehicle_make_freq(nyc_parking_2015)
top_5_vehicle_make_freq_2015

#Vehicle  Make   count
#1        FORD   1373157
#2        TOYOT  1082206
#3        HONDA  982130
#4        CHEVR  811659
#5        NISSA  805572

# Aggregate - Question 3

top_5_violating_precincts_freq_2015 <- get_top_5_violating_precincts_freq(nyc_parking_2015)
top_5_violating_precincts_freq_2015

#Violation Precinct   count
#1                  0 1455166
#2                 19  550797
#3                 18  393802
#4                 14  377750
#5                  1  302737

top_5_issuing_precincts_freq_2015 <- get_top_5_issuing_precincts_freq(nyc_parking_2015)
top_5_issuing_precincts_freq_2015

#Issuer Precinct   count
#1               0 1648671
#2              19  536627
#3              18  384863
#4              14  363734
#5               1  293942

top_3_issuing_precincts_2015 <- get_top_3_issuing_precincts(nyc_parking_2015)
top_3_issuing_precincts_2015 #(0,19,18)

# Aggregate - Question 4
# # 4. 
# Find the violation code frequency across 3 precincts which have issued the most number of tickets - 
# do these precinct zones have an exceptionally high frequency of certain violation codes? 
# Are these codes common across precincts?

violation_code_freq_top_3_precincts_2015 <- get_violation_code_freq_top_3_precincts(nyc_parking_2015 , top_3_issuing_precincts_2015)
violation_code_freq_top_3_precincts_2015

#These codes (38 , 36 and 14) are not exceptionally high and they are not common.

#Issuer Precinct Violation Code count_val rank
#1              19             38     89102    1
#2               0             36    747098    1
#3              18             14    119078    1

# Aggregate - Question 5
nyc_parking_2015 <- create_new_attribute_violation_time(nyc_parking_2015)
printSchema(nyc_parking_2015)

# New column violation_clock has been created out of the hour

cnt_missing_values <- get_tot_missing_values(nyc_parking_2015)
head(select(nyc_parking_2015 , cnt_missing_values)) #1446

head(select(nyc_parking_2015 , mean(nyc_parking_2015$violation_hour))) 
#Avg value is coming as 11.72

#hence we will replace the missing values of the hour with 11

nyc_parking_2015 <- replace_missing_values(nyc_parking_2015 ,11)
cnt_missing_values <- get_tot_missing_values(nyc_parking_2015)
head(select(nyc_parking_2015 , cnt_missing_values)) #0

nyc_parking_2015 <- create_bins(nyc_parking_2015)
printSchema(nyc_parking_2015) 
# created bins under violation_slot column

# calculating 3 most common violation across the 6 groups that we have created
# 
three_most_common_violations_2015 <- three_most_common_violations(nyc_parking_2015)
three_most_common_violations_2015

#Violation_slot Violation Code   count rank
#1         Evening             38  560017    1
#2         Evening             37  411856    2
#3         Evening             36  317230    3
#4   Early Morning             14  132344    1
#5   Early Morning             21  103874    2
#6   Early Morning             40   89612    3
#7       Afternoon             21 1167097    1
#8       Afternoon             38  442655    2
#9       Afternoon             36  353555    3
#10          Night              7   69973    1
#11          Night             38   61530    2
#12          Night             14   44570    3
#13    Pre_Morning             21   62056    1
#14    Pre_Morning             40   35114    2
#15    Pre_Morning             78   33483    3
#16   Late Evening             38  237513    1
#17   Late Evening             37  173008    2
#18   Late Evening             14  145602    3

top_3_violation_codes <- get_top_3_violaton_codes(nyc_parking_2015)
top_3_violation_codes #(21,38,14)

most_common_times_for_top_3_violation_2015 <- get_most_common_times_for_top_3_violation(nyc_parking_2015 , top_3_violation_codes)
most_common_times_for_top_3_violation_2015

#Violation Code Violation_slot count_val rank
#1             38        Evening    560017    1
#2             38      Afternoon    442655    2
#3             38   Late Evening    237513    3
#4             21      Afternoon   1167097    1
#5             21        Evening    134866    2
#6             21  Early Morning    103874    3
#7             14      Afternoon    292903    1
#8             14        Evening    267308    2
#9             14   Late Evening    145602    3

# Aggregate - Question 6
nyc_parking_2015 <- creating_seasons_from_year(nyc_parking_2015)
printSchema(nyc_parking_2015) # created a new column season

freq_tickets_season_2015 <- get_freq_tickets_season(nyc_parking_2015)
freq_tickets_season_2015

#season   count
#1 Spring 2860987
#2 Summer 2838306
#3 Autumn 2718502
#4 Winter 2180240

three_most_common_violations_season_2015 <- three_most_common_violations_season(nyc_parking_2015)
three_most_common_violations_season_2015

#season Violation Code  count rank
#1  Spring             21 425163    1
#2  Spring             38 327048    2
#3  Spring             14 243622    3
#4  Summer             21 439632    1
#5  Summer             38 344262    2
#6  Summer             14 239339    3
#7  Autumn             21 351390    1
#8  Autumn             38 326700    2
#9  Autumn             14 232300    3
#10 Winter             38 306997    1
#11 Winter             21 253043    2
#12 Winter             14 193157    3

# Aggregate - Question 7
# a) To find the top 3 violation codes

top3_violation_codes_2015 <- head(top_5_violation_code_freq_2015, n = 3)
top3_violation_codes_2015

#Violation Code   count
#1             21 1469228
#2             38 1305007
#3             14  908418

# b) Below are fines from NYC website for the above top 3 traffic violation codes
#     CODE    DEFINITION          Manhattan             All Other Areas       Average
#                                 96th St. & below
#       21  Street Cleaning       $65                   $45                   (65+45)/2 = $55
#       38  Muni Meter            $65                   $35                   (65+35)/2 = $50
#       14  General No Standing   $115                  $115                  (115+115)/2 = $115

# create a vector with average fines for the top 3 traffic violation codes
fines <- c(55,50,115)

# c) Find the total amount collected for all of the fines.
top3_violation_codes_fines_2015 <- top3_violation_codes_fines(top3_violation_codes_2015,fines)
top3_violation_codes_fines_2015

#Violation Code   count total_fine_amt
#1             21 1469228       80807540
#2             38 1305007       65250350
#3             14  908418      104468070

############################################################################################

###############
# For year 2016
###############

nyc_parking_2016 <- clean_prepare_data(nyc_parking_2016 , 2016)

total_tickets_2016 <- get_tot_tickets(nyc_parking_2016)
total_tickets_2016 # 10396894

unique_states_2016 <- get_cnt_uniq_states(nyc_parking_2016)
unique_states_2016 #68

cnt_empty_addr_2016 <- get_cnt_empty_addr_tickets(nyc_parking_2016)
cnt_empty_addr_2016 # 1963921

# Aggregation tasks - Question 1
top_5_violation_code_freq_2016 <- get_top_5_violaton_code_freq(nyc_parking_2016)
top_5_violation_code_freq_2016

#Violation Code   count
#1             21 1497269
#2             36 1232952
#3             38 1126835
#4             14  860045
#5             37  677805

# Aggregation tasks - Question 2
top_5_vehicle_body_type_freq_2016 <- get_top_5_vehicle_body_type_freq(nyc_parking_2016)
top_5_vehicle_body_type_freq_2016

#Vehicle Body Type   count
#1              SUBN 3393838
#2              4DSD 2936729
#3               VAN 1489924
#4              DELV  738747
#5               SDN  401750

top_5_vehicle_make_freq_2016 <- get_top_5_vehicle_make_freq(nyc_parking_2016)
top_5_vehicle_make_freq_2016

#Vehicle Make   count
#1         FORD 1297363
#2        TOYOT 1128909
#3        HONDA  991735
#4        NISSA  815963
#5        CHEVR  743416

# Aggregation tasks - Question 3
top_5_violating_precincts_freq_2016 <- get_top_5_violating_precincts_freq(nyc_parking_2016)
top_5_violating_precincts_freq_2016

#Violation Precinct   count
#1                  0 1807139
#2                 19  545669
#3                 18  325559
#4                 14  318193
#5                  1  299074

top_5_issuing_precincts_freq_2016 <- get_top_5_issuing_precincts_freq(nyc_parking_2016)
top_5_issuing_precincts_freq_2016

#Issuer Precinct   count
#1               0 2067219
#2              19  532298
#3              18  317451
#4              14  309727
#5               1  290472

top_3_issuing_precincts_2016 <- get_top_3_issuing_precincts(nyc_parking_2016)
top_3_issuing_precincts_2016 #(0,19,18)

# Aggregation tasks - Question 4
violation_code_freq_top_3_precincts_2016 <- get_violation_code_freq_top_3_precincts(nyc_parking_2016 , top_3_issuing_precincts_2016)
violation_code_freq_top_3_precincts_2016

#Issuer Precinct Violation Code count_val rank
#1              19             38     76178    1
#2               0             36   1232951    1
#3              18             14     98160    1

# Aggregation tasks - Question 5
nyc_parking_2016 <- create_new_attribute_violation_time(nyc_parking_2016)
printSchema(nyc_parking_2016)

cnt_missing_values <- get_tot_missing_values(nyc_parking_2016)
head(select(nyc_parking_2016 , cnt_missing_values)) #

head(select(nyc_parking_2016 , mean(nyc_parking_2016$violation_hour))) 
#Avg value is coming as 11.60

#hence we will replace the missing values of the hour with 11

nyc_parking_2016 <- replace_missing_values(nyc_parking_2016 ,11)
cnt_missing_values <- get_tot_missing_values(nyc_parking_2016)
head(select(nyc_parking_2016 , cnt_missing_values)) #0

nyc_parking_2016 <- create_bins(nyc_parking_2016)
printSchema(nyc_parking_2016) #violation_slot is the one.

three_most_common_violations_2016 <- three_most_common_violations(nyc_parking_2016)
three_most_common_violations_2016

#Violation_slot Violation Code   count rank
#1         Evening             36  536551    1
#2         Evening             38  480903    2
#3         Evening             37  378382    3
#4   Early Morning             14  137946    1
#5   Early Morning             21  110889    2
#6   Early Morning             40   89709    3
#7       Afternoon             21 1183378    1
#8       Afternoon             36  578036    2
#9       Afternoon             38  382101    3
#10          Night              7   56836    1
#11          Night             38   52582    2
#12          Night             40   43955    3
#13    Pre_Morning             21   66305    1
#14    Pre_Morning             40   36089    2
#15    Pre_Morning             78   28516    3
#16   Late Evening             38  208759    1
#17   Late Evening             37  159810    2
#18   Late Evening             14  132446    3

top_3_violation_codes <- get_top_3_violaton_codes(nyc_parking_2016)
top_3_violation_codes #(21,36,38)

most_common_times_for_top_3_violation_2016 <- get_most_common_times_for_top_3_violation(nyc_parking_2016 , top_3_violation_codes)
most_common_times_for_top_3_violation_2016

#Violation Code Violation_slot count_val rank
#1             38        Evening    480903    1
#2             38      Afternoon    382101    2
#3             38   Late Evening    208759    3
#4             21      Afternoon   1183378    1
#5             21        Evening    135686    2
#6             21  Early Morning    110889    3
#7             36      Afternoon    578036    1
#8             36        Evening    536551    2
#9             36  Early Morning     77657    3

# Aggregation tasks - Question 6
nyc_parking_2016 <- creating_seasons_from_year(nyc_parking_2016)
printSchema(nyc_parking_2016)

freq_tickets_season_2016 <- get_freq_tickets_season(nyc_parking_2016)
freq_tickets_season_2016

#season   count
#1 Spring 2789066
#2 Summer 2214536
#3 Autumn 2971672
#4 Winter 2421620

three_most_common_violations_season_2016 <- three_most_common_violations_season(nyc_parking_2016)
three_most_common_violations_season_2016

#season Violation Code  count rank
#1  Spring             21 383448    1
#2  Spring             36 374362    2
#3  Spring             38 299439    3
#4  Summer             21 358896    1
#5  Summer             38 255600    2
#6  Summer             14 200608    3
#7  Autumn             36 438320    1
#8  Autumn             21 395020    2
#9  Autumn             38 303387    3
#10 Winter             21 359905    1
#11 Winter             36 314765    2
#12 Winter             38 268409    3

# Aggregation tasks - Question 7
# a) To find the top 3 violation codes
top3_violation_codes_2016 <- head(top_5_violation_code_freq_2016, n = 3)
top3_violation_codes_2016

# Violation Code   count
#        21        1497269
#        36        1232952
#        38        1126835

# b) Below are fines from NYC website for the above top 3 traffic violation codes
#     CODE    DEFINITION            Manhattan             All Other Areas       Average
#                                   96th St. & below
#       21  Street Cleaning         $65                   $45                   (65+45)/2 = $55
#       36  Exceeding the posted    $50                   $50                   (50+50)/2 = $50
#           speed limit in or near 
#           a designated 
#           school zone.
#       38  Muni Meter              $65                   $35                   (65+35)/2 = $50

# create a vector with average fines for the top 3 traffic violation codes
fines <- c(55,50,50)

# c) Find the total amount collected for all of the fines.
top3_violation_codes_fines_2016 <- top3_violation_codes_fines(top3_violation_codes_2016,fines)
top3_violation_codes_fines_2016

#Violation Code   count total_fine_amt
#1             21 1497269       82349795
#2             36 1232952       61647600
#3             38 1126835       56341750

############################################################################################

# For year 2017

total_tickets_2017 <- get_tot_tickets(nyc_parking_2017)
total_tickets_2017 # 10539563

unique_states_2017 <- get_cnt_uniq_states(nyc_parking_2017)
unique_states_2017 #67

cnt_empty_addr_2017 <- get_cnt_empty_addr_tickets(nyc_parking_2017)
cnt_empty_addr_2017 # 2160639

# Aggregation tasks - Question 1
top_5_violation_code_freq_2017 <- get_top_5_violaton_code_freq(nyc_parking_2017)
top_5_violation_code_freq_2017

#Violation Code   count
#1             21 1500396
#2             36 1345237
#3             38 1050418
#4             14  880152
#5             20  609231

# Aggregation tasks - Question 2
top_5_vehicle_body_type_freq_2017 <- get_top_5_vehicle_body_type_freq(nyc_parking_2017)
top_5_vehicle_body_type_freq_2017

#Vehicle Body Type   count
#1              SUBN 3632003
#2              4DSD 3017372
#3               VAN 1384121
#4              DELV  672123
#5               SDN  414984

top_5_vehicle_make_freq_2017 <- get_top_5_vehicle_make_freq(nyc_parking_2017)
top_5_vehicle_make_freq_2017

#Vehicle Make   count
#1         FORD 1250777
#2        TOYOT 1179265
#3        HONDA 1052006
#4        NISSA  895225
#5        CHEVR  698024

# Aggregation tasks - Question 3
top_5_violating_precincts_freq_2017 <- get_top_5_violating_precincts_freq(nyc_parking_2017)
top_5_violating_precincts_freq_2017

#Violation Precinct   count
#1                  0 1950083
#2                 19  528317
#3                 14  347736
#4                  1  326961
#5                 18  302008

top_5_issuing_precincts_freq_2017 <- get_top_5_issuing_precincts_freq(nyc_parking_2017)
top_5_issuing_precincts_freq_2017

#Issuer Precinct   count
#1               0 2255086
#2              19  514786
#3              14  340862
#4               1  316776
#5              18  292237

top_3_issuing_precincts_2017 <- get_top_3_issuing_precincts(nyc_parking_2017)
top_3_issuing_precincts_2017 #(0,19,14)

# Aggregation tasks - Question 4
violation_code_freq_top_3_precincts_2017 <- get_violation_code_freq_top_3_precincts(nyc_parking_2017 , top_3_issuing_precincts_2017)
violation_code_freq_top_3_precincts_2017

#Issuer Precinct Violation Code count_val rank
#1              19             46     84789    1
#2              14             14     73007    1
#3               0             36   1345237    1

# Aggregation tasks - Question 5
nyc_parking_2017 <- create_new_attribute_violation_time(nyc_parking_2017)
printSchema(nyc_parking_2017)

cnt_missing_values <- get_tot_missing_values(nyc_parking_2017)
head(select(nyc_parking_2017 , cnt_missing_values)) #53

head(select(nyc_parking_2017 , mean(nyc_parking_2017$violation_hour))) 

#Avg value is coming as 11.6

#hence we will replace the missing values of the hour with 11

nyc_parking_2017 <- replace_missing_values(nyc_parking_2017 ,11)
cnt_missing_values <- get_tot_missing_values(nyc_parking_2017)
head(select(nyc_parking_2017 , cnt_missing_values)) #0

nyc_parking_2017 <- create_bins(nyc_parking_2017)
printSchema(nyc_parking_2017) #violation_slot is the one.

three_most_common_violations_2017 <- three_most_common_violations(nyc_parking_2017)
three_most_common_violations_2017

#Violation_slot Violation Code   count rank
#1         Evening             36  563564    1
#2         Evening             38  457289    2
#3         Evening             37  332479    3
#4   Early Morning             14  139373    1
#5   Early Morning             21  116862    2
#6   Early Morning             40  110631    3
#7       Afternoon             21 1161234    1
#8       Afternoon             36  726513    2
#9       Afternoon             38  343204    3
#10          Night              7   59134    1
#11          Night             38   46502    2
#12          Night             14   43894    3
#13    Pre_Morning             21   71728    1
#14    Pre_Morning             40   44732    2
#15    Pre_Morning             14   28730    3
#16   Late Evening             38  200785    1
#17   Late Evening             37  143593    2
#18   Late Evening             14  142317    3

top_3_violation_codes <- get_top_3_violaton_codes(nyc_parking_2017)
top_3_violation_codes #(21,36,38)

most_common_times_for_top_3_violation_2017 <- get_most_common_times_for_top_3_violation(nyc_parking_2017 , top_3_violation_codes)
most_common_times_for_top_3_violation_2017

#Violation Code Violation_slot count_val rank
#1             38        Evening    457289    1
#2             38      Afternoon    343204    2
#3             38   Late Evening    200785    3
#4             21      Afternoon   1161234    1
#5             21        Evening    149676    2
#6             21  Early Morning    116862    3
#7             36      Afternoon    726513    1
#8             36        Evening    563564    2
#9             36  Early Morning     30072    3

# Aggregation tasks - Question 6
nyc_parking_2017 <- creating_seasons_from_year(nyc_parking_2017)
printSchema(nyc_parking_2017)

freq_tickets_season_2017 <- get_freq_tickets_season(nyc_parking_2017)
freq_tickets_season_2017

#season   count
#1 Spring 2873383
#2 Summer 2353920
#3 Autumn 2829224
#4 Winter 2483036

three_most_common_violations_season_2017 <- three_most_common_violations_season(nyc_parking_2017)
three_most_common_violations_season_2017

#season Violation Code  count rank
#1  Spring             21 402424    1
#2  Spring             36 344834    2
#3  Spring             38 271167    3
#4  Summer             21 378699    1
#5  Summer             38 235725    2
#6  Summer             14 207495    3
#7  Autumn             36 456046    1
#8  Autumn             21 357257    2
#9  Autumn             38 283816    3
#10 Winter             21 362016    1
#11 Winter             36 359338    2
#12 Winter             38 259710    3

# Aggregation tasks - Question 7
# a) To find the top 3 violation codes
top3_violation_codes_2017 <- head(top_5_violation_code_freq_2017, n = 3)
top3_violation_codes_2017

# Violation Code   count
#        21        1500396
#        36        1345237
#        38        1050418

# b) Below are fines from NYC website for the above top 3 traffic violation codes
#     CODE    DEFINITION            Manhattan             All Other Areas       Average
#                                   96th St. & below
#       21  Street Cleaning         $65                   $45                   (65+45)/2 = $55
#       36  Exceeding the posted    $50                   $50                   (50+50)/2 = $50
#           speed limit in or near 
#           a designated 
#           school zone.
#       38  Muni Meter              $65                   $35                   (65+35)/2 = $50

# create a vector with average fines for the top 3 traffic violation codes
fines <- c(55,50,50)

# c) Find the total amount collected for all of the fines.
top3_violation_codes_fines_2017 <- top3_violation_codes_fines(top3_violation_codes_2017,fines)
top3_violation_codes_fines_2017

#Violation Code   count total_fine_amt
#1             21 1500396       82521780
#2             36 1345237       67261850
#3             38 1050418       52520900

############################################################
##                 Plots and Inference                    ##
############################################################

####################################################
# Function to generate plot for analysis
####################################################

gen_plot1 <- function(df , var1, var2, fill1 , title , xlab, filltitle) {
  ggplot( data = df , aes_string(x = var1 , y = var2, fill = fill1)) + 
    geom_col(position = "dodge") +
    labs(title = title , x = xlab , fill = filltitle) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.6)) 
}

#####################
# Aggregation tasks #
#####################

################################################################################################
# 1. How often does each violation code occur? (frequency of violation codes - find the top 5) #
################################################################################################

top_5_violation_code_freq_2015$year <- 2015
top_5_violation_code_freq_2016$year <- 2016
top_5_violation_code_freq_2017$year <- 2017

violation_code_list <- rbind(top_5_violation_code_freq_2015,top_5_violation_code_freq_2016,top_5_violation_code_freq_2017)
violation_code_list

gen_plot1(violation_code_list,"as.factor(`Violation Code`)","count","as.factor(year)","Top 5 violation codes across the year's",
          "Violation Code","year")

# Conclusion from the plot: We could see Violation Code '14', '21', '36' & '38' occuring across the 3 years as top violations. 
# Also we could see violation code ‘20’ is in top 5 only on 2017 and similarly ‘37’ is only on top 5 on year’s 2015 & 2016.   

########################################################################################################################
# 2. How often does each vehicle body type get a parking ticket? How about the vehicle make? (find the top 5 for both) #
########################################################################################################################

top_5_vehicle_body_type_freq_2015$year <- 2015
top_5_vehicle_body_type_freq_2016$year <- 2016
top_5_vehicle_body_type_freq_2017$year <- 2017

top5_vehicle_body_type <- rbind(top_5_vehicle_body_type_freq_2015,top_5_vehicle_body_type_freq_2016,top_5_vehicle_body_type_freq_2017)
top5_vehicle_body_type

top_5_vehicle_make_freq_2015$year <- 2015
top_5_vehicle_make_freq_2016$year <- 2016
top_5_vehicle_make_freq_2017$year <- 2017

top5_vehicle_make <- rbind(top_5_vehicle_make_freq_2015,top_5_vehicle_make_freq_2016,top_5_vehicle_make_freq_2017)
top5_vehicle_make

plt1 <- gen_plot1(top5_vehicle_body_type,"as.factor(`Vehicle Body Type`)","count","as.factor(year)","Top 5 vehicle body type across the year's",
                  "Vehicle Body Type","year")

plt2 <- gen_plot1(top5_vehicle_make,"as.factor(`Vehicle Make`)","count","as.factor(year)","Top 5 Vehicle Make across the year's",
                  "Vehicle Make","year")

grid.arrange(plt1,plt2,nrow=2, top = "EDA analysis for vehicle body type and vehicle make")

# Conclusion from the plot:  
# i) Vehicle Body Type – We could see the ‘SUBN’,‘4DSD’, ‘VAN’, ‘DELV’ & ‘SDN’ are the top 5 vehicle body types 
# got more parking tickets across the 3 years.
# ii) Vehicle Make – We could see the ‘FORD’,‘TOYOT’, ‘HONDA’, ‘NISSA’ & ‘CHEVR’ are the top 5 vehicle makes
# got more parking tickets across the 3 years.

###########################################################################################
# 3. A precinct is a police station that has a certain zone of the city under its command. 
# 3.1 
# Find the (5 highest) frequencies of:
# Violating Precincts (this is the precinct of the zone where the violation occurred)
###########################################################################################

top_5_violating_precincts_freq_2015$year <- 2015
top_5_violating_precincts_freq_2016$year <- 2016
top_5_violating_precincts_freq_2017$year <- 2017

top_5_violating_precincts <- rbind(top_5_violating_precincts_freq_2015,top_5_violating_precincts_freq_2016,top_5_violating_precincts_freq_2017)
top_5_violating_precincts

gen_plot1(top_5_violating_precincts,"as.factor(`Violation Precinct`)","count","as.factor(year)","Top 5 Violating Precincts across the year's",
          "Violation Precinct","year")

# Conclusion from the plot: The top 5 precincts which have more violations are ‘0’, ‘19’, ‘18’, ‘14’
# & ‘1’ across the 3 years.  

###################################################################
# 3.2 
# Find the (5 highest) frequencies of:
# Issuing Precincts (this is the precinct that issued the ticket)
###################################################################

top_5_issuing_precincts_freq_2015$year <- 2015
top_5_issuing_precincts_freq_2016$year <- 2016
top_5_issuing_precincts_freq_2017$year <- 2017

top_5_issuing_precincts <- rbind(top_5_issuing_precincts_freq_2015,top_5_issuing_precincts_freq_2016,top_5_issuing_precincts_freq_2017)
top_5_issuing_precincts

gen_plot1(top_5_issuing_precincts,"as.factor(`Issuer Precinct`)","count","as.factor(year)","Top 5 Issuing Precincts across the year's",
          "Issuer Precinct","year")

# Conclusion from the plot: The top 5 issuing precincts which have more violations are ‘0’, ‘19’, ‘18’,
# ‘14’ & ‘1’ across the 3 years.

##########################################################################################################
# 4. 
# Find the violation code frequency across 3 precincts which have issued the most number of tickets - 
# do these precinct zones have an exceptionally high frequency of certain violation codes? 
# Are these codes common across precincts?
##########################################################################################################

IP_plot1 <- gen_plot1(violation_code_freq_top_3_precincts_2015,"as.factor(`Violation Code`)","count_val","as.factor(`Issuer Precinct`)","Year - 2015",
                      "Violation Code","Issuer Precinct")

IP_plot2 <- gen_plot1(violation_code_freq_top_3_precincts_2016,"as.factor(`Violation Code`)","count_val","as.factor(`Issuer Precinct`)","Year - 2016",
                      "Violation Code","Issuer Precinct")

IP_plot3 <- gen_plot1(violation_code_freq_top_3_precincts_2017,"as.factor(`Violation Code`)","count_val","as.factor(`Issuer Precinct`)","Year - 2017",
                      "Violation Code","Issuer Precinct")

grid.arrange(IP_plot1,IP_plot2,IP_plot3,nrow=3, top = "Top Violation code across the top 3 Issuing Precincts across the year's")

# Conclusion from the plot: We could see that violation codes ‘14’, ‘36’ & ‘38’ for year 2015 an 2016 
# and violation codes ‘14’, ‘36’ & ‘46’. It is clear from the plot that not all the high frequency 
# violation codes are common across precincts and years.

##################################################################################################
# 5 .You’d want to find out the properties of parking violations across different times of the day:
##################################################################################################

# 5.1 The Violation Time field is specified in a strange format. 
# Find a way to make this into a time attribute that you can use to divide into groups

# 5.2 Find a way to deal with missing values, if any.

# 5.3 Divide 24 hours into 6 equal discrete bins of time. The intervals you choose are at your discretion. 
# For each of these groups, find the 3 most commonly occurring violations

slot_violation_plot1 <- gen_plot1(three_most_common_violations_2015,"as.factor(`Violation_slot`)","count","as.factor(`Violation Code`)","Year - 2015",
          "Violation slot","Violation Code")

slot_violation_plot2 <- gen_plot1(three_most_common_violations_2016,"as.factor(`Violation_slot`)","count","as.factor(`Violation Code`)","Year - 2016",
          "Violation slot","Violation Code")

slot_violation_plot3 <- gen_plot1(three_most_common_violations_2017,"as.factor(`Violation_slot`)","count","as.factor(`Violation Code`)","Year - 2017",
          "Violation slot","Violation Code")

grid.arrange(slot_violation_plot1,slot_violation_plot2 , slot_violation_plot3 ,nrow=3, top = "3 most violations across different slots of the day")

# 5.4 Now, try another direction. For the 3 most commonly occurring violation codes, 
# find the most common times of day (in terms of the bins from the previous part)

timeslot_plot1 <- gen_plot1(most_common_times_for_top_3_violation_2015,"as.factor(`Violation Code`)","count_val","as.factor(`Violation_slot`)","Year - 2015",
                            "Violation Code","Violation slot")

timeslot_plot2 <- gen_plot1(most_common_times_for_top_3_violation_2016,"as.factor(`Violation Code`)","count_val","as.factor(`Violation_slot`)","Year - 2016",
                            "Violation Code","Violation slot")


timeslot_plot3 <- gen_plot1(most_common_times_for_top_3_violation_2017,"as.factor(`Violation Code`)","count_val","as.factor(`Violation_slot`)","Year - 2017",
                            "Violation Code","Violation slot")

grid.arrange(timeslot_plot1,timeslot_plot2,timeslot_plot3,nrow=3, top = "Parking violations across different times of the day across the year's")

# Conclusion from the plot:  From the plots we can see the below:
# 1) Violation code ‘21’ occurs more during the afternoon time slot 
# 2) Lowest number of violations occur during Early Morning and Late Evening time slot

######################################################################################################
# 6. Let’s try and find some seasonality in this data
# First, divide the year into some number of seasons, and find frequencies of tickets for each season.
# Then, find the 3 most common violations for each of these season
#######################################################################################################

freq_tickets_season_2015$year <- 2015
freq_tickets_season_2016$year <- 2016
freq_tickets_season_2017$year <- 2017

freq_tickets_season_list <- rbind(freq_tickets_season_2015,freq_tickets_season_2016,freq_tickets_season_2017)
freq_tickets_season_list

gen_plot1(freq_tickets_season_list,"as.factor(`season`)","count","as.factor(year)","Total violations across seasons / year",
          "season","year")


season_freq_plot1 <- gen_plot1(freq_tickets_season_2015,"as.factor(`Violation Code`)","count","as.factor(`season`)","Year - 2015",
                          "Violation Code","season")

season_plot1 <- gen_plot1(three_most_common_violations_season_2015,"as.factor(`Violation Code`)","count","as.factor(`season`)","Year - 2015",
                          "Violation Code","season")

season_plot2 <- gen_plot1(three_most_common_violations_season_2016,"as.factor(`Violation Code`)","count","as.factor(`season`)","Year - 2016",
                          "Violation Code","season")

season_plot3 <- gen_plot1(three_most_common_violations_season_2017,"as.factor(`Violation Code`)","count","as.factor(`season`)","Year - 2017",
                          "Violation Code","season")

grid.arrange(season_plot1,season_plot2,season_plot3,nrow=3, top = "Top 3 Parking violations across different seasons for the 3 years")

# Conclusion from the plot: From the plot, we could see that violation codes ‘21’ and ‘38’ 
# is common across all the seasons and all the 3 years. However violation codes ‘14’ and ‘36’ 
# had different behavior across seasons and the years.

#####################################################################################################################################################################
# 7. The fines collected from all the parking violation constitute a revenue source for the NYC police department.
# Let’s take an example of estimating that for the 3 most commonly occurring codes.
#
# a) Find total occurrences of the 3 most common violation codes
# b) Then, search the internet for NYC parking violation code fines. You will find a website (on the nyc.gov URL) that lists these fines.
#    They’re divided into two categories, one for the highest-density locations of the city, the other for the rest of the city. 
#    For simplicity, take an average of the two.
# c) Using this information, find the total amount collected for all of the fines. State the code which has the highest total collection.
# d) What can you intuitively infer from these findings?
######################################################################################################################################################################

top3_violation_codes_fines_2015$year <- 2015
top3_violation_codes_fines_2016$year <- 2016
top3_violation_codes_fines_2017$year <- 2017

top3_violation_codes_fines <- rbind(top3_violation_codes_fines_2015,top3_violation_codes_fines_2016,top3_violation_codes_fines_2017)
top3_violation_codes_fines

gen_plot1(top3_violation_codes_fines,"as.factor(`Violation Code`)","total_fine_amt","as.factor(year)","Top 3 total fine amount collected across the year's",
          "Violation Code","year")

# Conclusion from the plot: From the plot, we could see violation code ‘14’ on year 2015 had the highest fine collection
# and this code this not in the top 3 for 2016 and 2017. Also we could see violation code ‘21’ the common violation code 
# which provides the next highest fine collection and this violation code is common across all the 3 years.

############################# END #############################