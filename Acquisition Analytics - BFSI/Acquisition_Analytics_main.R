###########################################################
#   Case study - Acquisition Analytics (BFSI)             #
#   Name: Manohar Shanmugasundaram                        #
#   Roll Id: DDA1730068                                   #
###########################################################

# Loading the required libraries for this assignment problem
library(ggplot2)
library(caret)
library(caTools)
library(dummies)
library(MASS)
library(car)
library(dplyr)
library(ROCR)
library(cowplot)

# Loading bank marketing data in the working directory. 
bank_data<- read.csv("bank_marketing.csv")

#--------------------------------------------------------------------------
## Business and Data understanding using  Exploratory Data Analysis (EDA)
#--------------------------------------------------------------------------

# Checking structure of dataset 
str(bank_data)

# Summary of dataset
summary(bank_data)

# Checking response rate of prospect customer
response <- 4640/(36548+4640)
response

# Checking missing values
sum(is.na(bank_data))

# Plotting Age histogram
ggplot(bank_data,aes(age))+geom_histogram()

# Let's check the outlier in the variables 
quantile(bank_data$age,seq(0,1,0.01))

# Box plot 
boxplot(bank_data$age)

# Capping the upper values of age with 71.
bank_data[(which(bank_data$age>71)),]$age <- 71

# Binning the age variable and store it into "binning.age".
bank_data$binning.age <- as.factor(cut(bank_data$age, breaks = c(16, 20, 30, 40, 50, 60, 70, 80)))

# Change the response value to numbers i.e"yes-no" to "1-0"
bank_data$response <- ifelse(bank_data$response == "yes", 1, 0)

# Check the numeric value of response rate in each bucket
agg_age <- merge(aggregate(response ~ binning.age, bank_data, mean),aggregate(response~binning.age, bank_data, sum),by = "binning.age") 

# Adding No.of_prospect
count <- data.frame(table(bank_data$binning.age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)

# changing column name of each variables in agg_age dataframe
colnames(agg_age) <- c("age", "response_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_age$response_rate <- format(round(agg_age$response_rate, 2))
agg_age

# Let's see the response rate of each age bucket in the plot
ggplot(agg_age, aes(age, No.of_prospect,label = response_rate)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

# Let's check the dataset of age less than 20 years. 
Bank_data_age20 <- subset(bank_data,age <20)

View(Bank_data_age20)
summary(Bank_data_age20)

##--------------------------------------------------------  

# Checking structure of dataset
str(bank_data)

#-----Next Variable is "job"

# Checking the levels of the job
levels(bank_data$job)

# Plotting bar graph for job variable.

# Writing a function "plot_response" to do the same task for each variable
plot_response <- function(cat_var, var_name){
  a <- aggregate(response~cat_var, bank_data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  
  colnames(agg_response) <- c(var_name, "response_rate","No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  
  ggplot(agg_response, aes(agg_response[, 1], count, label = response_rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
}

plot_response(bank_data$job, "job")

##--------------------------------------------------------  

# Checking structure of dataset 
str(bank_data)

# Checking Marital status
summary(bank_data$marital)

# Let's replace Unknown level to married
levels(bank_data$marital)[4] <- "married"

# Plotting marital status
plot_response(bank_data$marital,"marital")

# Let's see the education variables
plot_response(bank_data$education,"Education")

# Reducing the levels of education variable
levels(bank_data$education)[c(1:3,5)] <- "Primary_Education"
levels(bank_data$education)[2] <- "Secondary_Education"
levels(bank_data$education)[4]<- "Tertiary_Education"

# Let's again check the education plot
plot_response(bank_data$education,"Education_levels")

#-------------------------------------------------------
# Let's see the default variable
table(bank_data$default)

plot_response(bank_data$default, "Default")
bank_data <- bank_data[,-5]

#-------------------------------------------------------

# Let's understand the housing variables 
summary(bank_data$housing)

plot_response(bank_data$housing, "Housing")

#-------------------------------------------------------

#-- Let's see the next variable which is "loan"
summary(bank_data$loan)

plot_response(bank_data$loan, "Loan Status")
#-------------------------------------------------------

#  Next variable is Contact, Let's see the response rate of each mode 
summary(bank_data$contact)
plot_response(bank_data$contact,"Contact_mode")

#-------------------------------------------------------

# Next variable is "Month" i.e contact month. 
plot_response(bank_data$month,"Contact_month")

#-------------------------------------------------------

# Let's do the same of "day_of_week" variable
plot_response(bank_data$day_of_week,"day_of_week")

#-------------------------------------------------------

# Now, Let's see the "duration" variable: Which is Quantitative variable

# Let's check the histogram 
ggplot(bank_data,aes(duration))+geom_histogram()

# Let's see the summary of this variable once 
summary(bank_data$duration)

# Average duration 
bank_data$response_1 <- as.factor(bank_data$response)
Avg_duration <- aggregate(duration~response_1,bank_data,mean)

bank_data <- bank_data[,-22]

## Definitely the outlier is present in the dataset

# So let's check the percentile distribution of duration 
quantile(bank_data$duration,seq(0,1,0.01))

# So, capping the duration seconds at 99% which is 1271.3sec 
bank_data[(which(bank_data$duration>1271.13)),]$duration <- 1271.13

# Now, again plot the histogram 
ggplot(bank_data,aes(duration))+geom_histogram()

#-------------------------------------------------------

# the next variable is "campaign" variable
#(number of contacts performed during this campaign and for this client 
# numeric, includes last contact)

# So let's check the summay of this variable 
summary(bank_data$campaign)

# Let's see the percentile distribution of this variable
boxplot(bank_data$campaign)

quantile(bank_data$campaign,seq(0,1,0.01))

# Capping this at 99% which the value is 14
bank_data[which(bank_data$campaign>14),]$campaign <- 14

# Visualizing it with plot
ggplot(bank_data,aes(campaign))+geom_histogram()

#-------------------------------------------------------
#-- Next variable is "pdays"
# Let's first convert this variable to factor type
bank_data$pdays<- as.factor(bank_data$pdays)

# Checking summary
summary(bank_data$pdays)

levels(bank_data$pdays)

# Reducing the levels of this variable to 3.
levels(bank_data$pdays)[1:10] <- "Contacted_in_first_10days"
levels(bank_data$pdays)[2:17] <-"Contacted_after_10days"
levels(bank_data$pdays)[3] <- "First_time_contacted"

# Also,lets see the respose rate of each levels. 
plot_response(bank_data$pday,"Pday")

# Number of prospects under each category
table(bank_data$pdays)

#-------------------------------------------------------

# Next variable is "previous" i.e number of contacts performed before 
# this campaign and for this client (numeric)
summary(bank_data$previous)

# Max=7, best is to convert this variable to factor
bank_data$previous <- as.factor(bank_data$previous)

levels(bank_data$previous)[1]<-"Never contacted"
levels(bank_data$previous)[2:4] <- "Less_than_3_times"
levels(bank_data$previous)[3:6] <- "More than_3_times"

summary(bank_data$previous)

plot_response(bank_data$previous,"Previous_contacts")

# Now, the next variable is "Poutcome" i.e  outcome of the previous marketing campaign 
# (categorical: 'failure','nonexistent','success')
summary(bank_data$poutcome)

plot_response(bank_data$poutcome,"Outcome_of_Previous_contacts")

#-------------------------------------------------------

#-- social and economic context attributes

# emp.var.rate- :employment variation rate - quarterly indicator (numeric)
summary(bank_data$emp.var.rate)

# Histogram of employment variation rate variable
ggplot(bank_data,aes(emp.var.rate))+geom_histogram()

# cons.price.idx:consumer price index - monthly indicator (numeric) 
summary(bank_data$cons.price.idx)

# Histogram of consumer price index variable
ggplot(bank_data,aes(cons.price.idx))+geom_histogram()

# cons.conf.idx: consumer confidence index - monthly indicator (numeric) 
summary(bank_data$cons.conf.idx)

# euribor3m: euribor 3 month rate - daily indicator (numeric)
summary(bank_data$euribor3m)

# nr.employed: number of employees - quarterly indicator (numeric)
summary(bank_data$nr.employed)

######## Add the new column Prospect_id in the data frame #########

bank_data$prospect_id <- seq.int(nrow(bank_data))

##############################################
#             Model Building                ## 
##----------Logistic Regression--------------#
##############################################

#---------------------------------------------------------    

# Task 2 - Build a logistic regression model without using the variable 'duration'
# i) Perform variable selection using the usual methods
# ii) Sort the data points in decreasing order of probability of response
# iii) Find the optimal probability cut-off and report the relevant evaluation metrics

##########################################################################################################
# Commented the following code of removal of the bining variable. I would like to try the model using the 
# binning variable as well
###########################################################################################################
# Removing binning variables 
# summary(bank_data[,21])
# colnames(bank_data[,21]) <- "age_group"
# bank_data <- bank_data[, -21]

#creating dummy variables
bank_data$response <- as.integer(bank_data$response)

bank_data_original <- bank_data

bank_data <- dummy.data.frame(bank_data)

bank_data$response <- as.factor(ifelse(bank_data$response == 1, "yes", "no"))

#---------------------------------------------------------    

# splitting into train and test data
set.seed(1)

split_indices <- sample.split(bank_data$response, SplitRatio = 0.70)

train <- bank_data[split_indices, ]

test <- bank_data[!split_indices, ]

nrow(train)/nrow(bank_data)

nrow(test)/nrow(bank_data)

#---------------------------------------------------------    

### Model 1: Logistic Regression

model_1 <- glm(response ~ ., family = "binomial", data = train)

summary(model_1)

#---------------------------------------------------------    

# Using stepwise algorithm for removing insignificant variables 

# model_2 <- stepAIC(model_1, direction = "both") 
# commented to avoid the code running longer and instead provided the below output model from stepAIC

model_2 <- glm(formula = response ~ age + jobadmin. + jobretired + jobstudent + 
            jobtechnician + jobunemployed + educationprofessional.course + 
            educationTertiary_Education + contactcellular + monthapr + 
            monthaug + monthjun + monthmar + monthmay + monthnov + monthoct + 
            day_of_weekfri + day_of_weekmon + day_of_weekthu + duration + 
            campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
            previousLess_than_3_times + poutcomefailure + emp.var.rate + 
            cons.price.idx + cons.conf.idx + euribor3m + nr.employed + 
            `binning.age(16,20]` + `binning.age(20,30]` + `binning.age(30,40]` + 
            `binning.age(40,50]` + `binning.age(50,60]` + prospect_id, 
            family = "binomial", data = train)

vif(model_2)
summary(model_2)

################ Remove variable Duration and prospect_id from the model building ##############
# Removed the variable 'duration'. We cannot use this variable since this 
# doesn't abtained from the marketing procurement data and this cannot be 
# predictor as well, since a long duration call will increase the marketing
# cost significantly.

## Removed the 'prospect_id' from the model, since we know that this variable cannot contribute to response

model_3 <- glm(formula = response ~ age + jobadmin. + jobretired + jobstudent + 
                 jobtechnician + jobunemployed + educationprofessional.course + 
                 educationTertiary_Education + contactcellular + monthapr + 
                 monthaug + monthjun + monthmar + monthmay + monthnov + monthoct + 
                 day_of_weekfri + day_of_weekmon + day_of_weekthu + 
                 campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                 previousLess_than_3_times + poutcomefailure + emp.var.rate + 
                 cons.price.idx + cons.conf.idx + euribor3m + nr.employed + 
                 `binning.age(16,20]` + `binning.age(20,30]` + `binning.age(30,40]` + 
                 `binning.age(40,50]` + `binning.age(50,60]`, 
                  family = "binomial", data = train)

vif(model_3)
summary(model_3)

# Remove variable 'emp.var.rate' 
model_4 <- glm(formula = response ~ age + jobadmin. + jobretired + jobstudent + 
              jobtechnician + jobunemployed + educationprofessional.course + 
              educationTertiary_Education + contactcellular + monthapr + 
              monthaug + monthjun + monthmar + monthmay + monthnov + monthoct + 
              day_of_weekfri + day_of_weekmon + day_of_weekthu + 
              campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
              previousLess_than_3_times + poutcomefailure +  
              cons.price.idx + cons.conf.idx + euribor3m + nr.employed + 
              `binning.age(16,20]` + `binning.age(20,30]` + `binning.age(30,40]` + 
              `binning.age(40,50]` + `binning.age(50,60]`, 
              family = "binomial", data = train)
vif(model_4)
summary(model_4)

# Remove variable 'euribor3m' 
model_5 <- glm(formula = response ~ age + jobadmin. + jobretired + jobstudent + 
              jobtechnician + jobunemployed + educationprofessional.course + 
              educationTertiary_Education + contactcellular + monthapr + 
              monthaug + monthjun + monthmar + monthmay + monthnov + monthoct + 
              day_of_weekfri + day_of_weekmon + day_of_weekthu + 
              campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
              previousLess_than_3_times + poutcomefailure +  cons.price.idx + cons.conf.idx + nr.employed + 
              `binning.age(16,20]` + `binning.age(20,30]` + `binning.age(30,40]` + 
              `binning.age(40,50]` + `binning.age(50,60]`, 
              family = "binomial", data = train)

vif(model_5)
summary(model_5)

# Remove variable 'binning.age(20,30]' 
model_6 <- glm(formula = response ~ age + jobadmin. + jobretired + jobstudent + 
                 jobtechnician + jobunemployed + educationprofessional.course + 
                 educationTertiary_Education + contactcellular + monthapr + 
                 monthaug + monthjun + monthmar + monthmay + monthnov + monthoct + 
                 day_of_weekfri + day_of_weekmon + day_of_weekthu + 
                 campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                 previousLess_than_3_times + poutcomefailure +  cons.price.idx + cons.conf.idx + nr.employed + 
                 `binning.age(16,20]` + `binning.age(30,40]` + `binning.age(40,50]` + `binning.age(50,60]`, 
                 family = "binomial", data = train)

vif(model_6)
summary(model_6)

# Remove variable 'previousLess_than_3_times' 
model_7 <- glm(formula = response ~ age + jobadmin. + jobretired + jobstudent + 
                 jobtechnician + jobunemployed + educationprofessional.course + 
                 educationTertiary_Education + contactcellular + monthapr + 
                 monthaug + monthjun + monthmar + monthmay + monthnov + monthoct + 
                 day_of_weekfri + day_of_weekmon + day_of_weekthu + 
                 campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                 poutcomefailure +  cons.price.idx + cons.conf.idx + nr.employed + 
                 `binning.age(16,20]` + `binning.age(30,40]` + `binning.age(40,50]` + `binning.age(50,60]`, 
                 family = "binomial", data = train)
vif(model_7)
summary(model_7)

# Remove variable 'monthapr' 
model_8 <- glm(formula = response ~ age + jobadmin. + jobretired + jobstudent + 
                 jobtechnician + jobunemployed + educationprofessional.course + 
                 educationTertiary_Education + contactcellular + 
                 monthaug + monthjun + monthmar + monthmay + monthnov + monthoct + 
                 day_of_weekfri + day_of_weekmon + day_of_weekthu + 
                 campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                 poutcomefailure +  cons.price.idx + cons.conf.idx + nr.employed + 
                 `binning.age(16,20]` + `binning.age(30,40]` + `binning.age(40,50]` + `binning.age(50,60]`, 
                 family = "binomial", data = train)

vif(model_8)
summary(model_8)

# Remove variable 'day_of_weekthu' 
model_9 <- glm(formula = response ~ age + jobadmin. + jobretired + jobstudent + 
              jobtechnician + jobunemployed + educationprofessional.course + 
              educationTertiary_Education + contactcellular + 
              monthaug + monthjun + monthmar + monthmay + monthnov + monthoct + 
              day_of_weekfri + day_of_weekmon + campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
              poutcomefailure +  cons.price.idx + cons.conf.idx + nr.employed + 
              `binning.age(16,20]` + `binning.age(30,40]` + `binning.age(40,50]` + `binning.age(50,60]`, 
              family = "binomial", data = train)

vif(model_9)
summary(model_9)

# Remove variable '`binning.age(16,20]`' 
model_10 <- glm(formula = response ~ age + jobadmin. + jobretired + jobstudent + 
                 jobtechnician + jobunemployed + educationprofessional.course + 
                 educationTertiary_Education + contactcellular + 
                 monthaug + monthjun + monthmar + monthmay + monthnov + monthoct + 
                 day_of_weekfri + day_of_weekmon + campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                 poutcomefailure +  cons.price.idx + cons.conf.idx + nr.employed + 
                 `binning.age(30,40]` + `binning.age(40,50]` + `binning.age(50,60]`, 
                 family = "binomial", data = train)
vif(model_10)
summary(model_10)

# Remove variable 'jobunemployed' 
model_11 <- glm(formula = response ~ age + jobadmin. + jobretired + jobstudent + 
                  jobtechnician + educationprofessional.course + educationTertiary_Education + contactcellular + 
                  monthaug + monthjun + monthmar + monthmay + monthnov + monthoct + 
                  day_of_weekfri + day_of_weekmon + campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                  poutcomefailure +  cons.price.idx + cons.conf.idx + nr.employed + 
                  `binning.age(30,40]` + `binning.age(40,50]` + `binning.age(50,60]`, 
                  family = "binomial", data = train)

vif(model_11)
summary(model_11)

# Remove variable 'age' 
model_12 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                  jobtechnician + educationprofessional.course + educationTertiary_Education + contactcellular + 
                  monthaug + monthjun + monthmar + monthmay + monthnov + monthoct + 
                  day_of_weekfri + day_of_weekmon + campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                  poutcomefailure +  cons.price.idx + cons.conf.idx + nr.employed + 
                  `binning.age(30,40]` + `binning.age(40,50]` + `binning.age(50,60]`, 
                  family = "binomial", data = train)

vif(model_12)
summary(model_12)

# Remove variable 'monthoct' 
model_13 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                  jobtechnician + educationprofessional.course + educationTertiary_Education + contactcellular + 
                  monthaug + monthjun + monthmar + monthmay + monthnov +
                  day_of_weekfri + day_of_weekmon + campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                  poutcomefailure +  cons.price.idx + cons.conf.idx + nr.employed + 
                  `binning.age(30,40]` + `binning.age(40,50]` + `binning.age(50,60]`, 
                  family = "binomial", data = train)

vif(model_13)
summary(model_13)

# Remove variable 'educationprofessional.course' 
model_14 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                  jobtechnician + educationTertiary_Education + contactcellular + monthaug + monthjun + monthmar + monthmay + 
                  monthnov + day_of_weekfri + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                  pdaysContacted_after_10days + poutcomefailure +  cons.price.idx + cons.conf.idx + nr.employed + 
                  `binning.age(30,40]` + `binning.age(40,50]` + `binning.age(50,60]`, 
                  family = "binomial", data = train)

vif(model_14)
summary(model_14)

# Remove variable 'day_of_weekfri' 
model_15 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                  jobtechnician + educationTertiary_Education + contactcellular + monthaug + monthjun + monthmar + monthmay + 
                  monthnov + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                  pdaysContacted_after_10days + poutcomefailure +  cons.price.idx + cons.conf.idx + nr.employed + 
                  `binning.age(30,40]` + `binning.age(40,50]` + `binning.age(50,60]`, 
                  family = "binomial", data = train)

vif(model_15)
summary(model_15)

# Remove variable '`binning.age(50,60]`' 
model_16 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                  jobtechnician + educationTertiary_Education + contactcellular + monthaug + monthjun + monthmar + monthmay + 
                  monthnov + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                  pdaysContacted_after_10days + poutcomefailure +  cons.price.idx + cons.conf.idx + nr.employed + 
                  `binning.age(30,40]` + `binning.age(40,50]`, family = "binomial", data = train)

vif(model_16)
summary(model_16)

# Remove variable '`binning.age(40,50]`' 
model_17 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                  jobtechnician + educationTertiary_Education + contactcellular + monthaug + monthjun + monthmar + monthmay + 
                  monthnov + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                  pdaysContacted_after_10days + poutcomefailure +  cons.price.idx + cons.conf.idx + nr.employed + 
                  `binning.age(30,40]`, family = "binomial", data = train)

vif(model_17)
summary(model_17)

# Remove variable '`binning.age(30,40]`' 
model_18 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                  jobtechnician + educationTertiary_Education + contactcellular + monthaug + monthjun + monthmar + monthmay + 
                  monthnov + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                  pdaysContacted_after_10days + poutcomefailure +  cons.price.idx + cons.conf.idx + 
                  nr.employed, family = "binomial", data = train)

vif(model_18)
summary(model_18)

# Remove variable 'jobadmin.' 
model_19 <- glm(formula = response ~ jobretired + jobstudent + 
                  jobtechnician + educationTertiary_Education + contactcellular + monthaug + monthjun + monthmar + monthmay + 
                  monthnov + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                  pdaysContacted_after_10days + poutcomefailure +  cons.price.idx + cons.conf.idx + 
                  nr.employed, family = "binomial", data = train)

vif(model_19)
summary(model_19)

# Remove variable 'jobtechnician' 
model_20 <- glm(formula = response ~ jobretired + jobstudent + 
                  educationTertiary_Education + contactcellular + monthaug + monthjun + monthmar + monthmay + 
                  monthnov + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                  pdaysContacted_after_10days + poutcomefailure +  cons.price.idx + cons.conf.idx + 
                  nr.employed, family = "binomial", data = train)

vif(model_20)
summary(model_20)

# Remove variable 'monthaug' 
model_21 <- glm(formula = response ~ jobretired + jobstudent + 
                  educationTertiary_Education + contactcellular + monthjun + monthmar + monthmay + 
                  monthnov + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                  pdaysContacted_after_10days + poutcomefailure +  cons.price.idx + cons.conf.idx + 
                  nr.employed, family = "binomial", data = train)

vif(model_21)
summary(model_21)

# Remove variable 'cons.conf.idx' 
model_22 <- glm(formula = response ~ jobretired + jobstudent + 
                  educationTertiary_Education + contactcellular + monthjun + monthmar + monthmay + 
                  monthnov + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                  pdaysContacted_after_10days + poutcomefailure + cons.price.idx + 
                  nr.employed, family = "binomial", data = train)

vif(model_22)
summary(model_22)

# Remove variable 'educationTertiary_Education' 
model_23 <- glm(formula = response ~ jobretired + jobstudent + contactcellular + monthjun + monthmar + monthmay + 
                  monthnov + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                  pdaysContacted_after_10days + poutcomefailure + cons.price.idx + 
                  nr.employed, family = "binomial", data = train)

vif(model_23)
summary(model_23)

# Remove variable 'cons.price.idx' 
model_24 <- glm(formula = response ~ jobretired + jobstudent + contactcellular + monthjun + monthmar + monthmay + 
                  monthnov + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                  pdaysContacted_after_10days + poutcomefailure + nr.employed, 
                  family = "binomial", data = train)

vif(model_24)
summary(model_24)

# Remove variable 'jobstudent' 
model_25 <- glm(formula = response ~ jobretired + contactcellular + monthjun + monthmar + monthmay + 
                  monthnov + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                  pdaysContacted_after_10days + poutcomefailure + nr.employed, 
                family = "binomial", data = train)

vif(model_25)
summary(model_25)

logistic_final <- model_25

# Predicting probabilities of responding for the test data
predictions_logit <- predict(logistic_final, newdata = test[, -61], type = "response")
summary(predictions_logit)

#--------------------------------------------------------- 

## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.
predicted_response <- factor(ifelse(predictions_logit >= 0.50, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.
conf <- confusionMatrix(predicted_response, test$response, positive = "yes")

conf

# Accuracy    : 0.8992 
# Sensitivity : 0.22270         
# Specificity : 0.98504

#---------------------------------------------------------    

# Let's find out the optimal probalility cutoff 
perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out) }

summary(test$response)
#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.
s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

#---------------------------------------------------------    

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.09)]
cutoff
# 0.07929293

# Let's choose a cutoff value of 7.9% for final model
predicted_response <- factor(ifelse(predictions_logit >= 0.07929293, "yes", "no"))

conf_final <- confusionMatrix(predicted_response, test$response, positive = "yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc
# 0.7593882

sens
# 0.6824713

spec
# 0.7691536

##########################################
# MODEL EVALUATION using KS - statistic  #
##########################################

## KS -statistic - Test Data
test_cutoff_attr <- predicted_response
test_actual_attr <- test$response

test_cutoff_attr <- ifelse(test_cutoff_attr=="yes",1,0)
test_actual_attr <- ifelse(test_actual_attr=="yes",1,0)

pred_object <- prediction(test_cutoff_attr, test_actual_attr)

performance_measures_test<- performance(pred_object, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) #0.4516249

# KS statistic for the model is 0.4516249. Hence we can say that KS statistic for the model is nearly 45%

### Task 3 - Create a data frame with the variables prospect ID, actual response, predicted response, 
####         predicted probability of response, duration of call in seconds, and cost of call

# While creating the data frame, calculate the cost of call for each prospect in a new column

test$predicted_prob <- predictions_logit
test$predicted_response <- predicted_response

test_predictions_dt <- test[, c("prospect_id","response", "predicted_prob", "predicted_response","duration")]

# New column cost of call using the formula provided below
test_predictions_dt$costofcall <- 0.033*(test_predictions_dt$duration) + 0.8
  
head(test_predictions_dt,5)

lift <- function(labels , predicted_prob, groups=10) {
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

# Create a Table of cumulative gain and lift 

test_predictions_dt$response <- as.factor(ifelse(test_predictions_dt$response=="yes",1,0))
test_predictions_dt$predicted_response <- as.factor(ifelse(test_predictions_dt$predicted_response=="yes",1,0))

LG = lift(test_predictions_dt$response, test_predictions_dt$predicted_prob, groups = 10)
View(LG)

## Task 4 - Find the number of top X% prospects you should target to meet the business objective

# Gain Chart
plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response") 

## Result -- From the above GAIN chart, we can infer that by focussing on the top 50% of the records (after sorting them by probabilities)
## we can reach out to 80% of the responders.

## Report the average call duration for targeting the top X% prospects to the CMO (report this as a comment in the R file)

# As we need to identified to achieve the business coverage of 80% responsders, we need to target top 50% of records. 
# Hence dividing the test records by 2 to get the count of 50%
number_prospects_in_80per <- nrow(test_predictions_dt) / 2
number_prospects_in_80per

# Once we got the 50% record count, sort the test data based on predicted probability (descending) and select the only the top 50% records
# This will give the records only the top 5 deciles
Top5_deciles <- head(test_predictions_dt[order(-test_predictions_dt$predicted_prob),],number_prospects_in_80per)

# Average call duration for targeting the top 80% prospect.
mean(Top5_deciles$duration)
# 263.6367 (seconds) i.e., 4.39 minutes

# Average call cost for targeting the top 80% prospect.
mean(Top5_deciles$costofcall)
# 9.500011 (INR)

# Task 5 - Create a lift chart
# The x-axis contains the number of prospects contacted; the y-axis contains the ratio:
# response rate using the model/ response rate without using the model

# Lift Chart 
ggplot(data = LG, aes(x=bucket,y=Cumlift))+ 
  geom_line(color = 'red')+geom_point()+ 
  ggtitle("Lift Chart") + labs(y = "Lift") + labs(x = "% of total targeted") + 
  scale_x_continuous(breaks = c(seq(1:10))) + scale_y_continuous(breaks = c(seq(1:5))) + 
  geom_hline(yintercept = 1.60) +
  geom_vline(xintercept = 5) +
  box() + panel_border(colour = "black")

#########################################################################
#From the above lift chart, we can see the cummulative lift at the end of
# 1st decile is 4.42
# 2nd decile is 3.09
# 3rd decile is 2.31
# 4th decile is 1.86
# 5th decile is 1.60
#########################################################################

# The Cumulative Lift of 1.60 for 5 deciles, means that when selecting 80% of the records based on the model, 
# one can expect 1.60 times the total number of targets (events) found by randomly. So 5 * 1.60 = 8 (will give 80% of
# responders).

############################### Conclusion ####################################################### 
# Targeting the 50% of the top probabilities will enable reaching out to 80% of the responsders  #
# Average call duration is 263.6 seconds and the average call cost for reaching out to these     #
# responders is 9.5 INR                                                                          #  
##################################################################################################

################# END #################
